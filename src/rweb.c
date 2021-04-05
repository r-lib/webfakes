
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include "civetweb.h"
#include "errors.h"
#include "cleancall.h"

#include <pthread.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <poll.h>
#endif

/* --------------------------------------------------------------------- */
/* registration                                                          */
/* --------------------------------------------------------------------- */

SEXP server_start(SEXP options);
SEXP server_poll(SEXP rsrv, SEXP clean);
SEXP server_stop(SEXP rsrv);
SEXP server_get_ports(SEXP rsrv);

SEXP response_delay(SEXP req, SEXP secs);
SEXP response_send_headers(SEXP req);
SEXP response_send(SEXP req);
SEXP response_write(SEXP req, SEXP data);
SEXP response_send_error(SEXP req, SEXP message, SEXP status);
SEXP response_send_chunk(SEXP req, SEXP data);

SEXP webfakes_crc32(SEXP v);

static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,

  /* server */
  { "server_start",          (DL_FUNC) &server_start,          1 },
  { "server_poll",           (DL_FUNC) &server_poll,           2 },
  { "server_stop",           (DL_FUNC) &server_stop,           1 },
  { "server_get_ports",      (DL_FUNC) &server_get_ports,      1 },

  /* request/response/connection */
  { "response_delay",        (DL_FUNC) &response_delay,        2 },
  { "response_send_headers", (DL_FUNC) &response_send_headers, 1 },
  { "response_send",         (DL_FUNC) &response_send,         1 },
  { "response_write",        (DL_FUNC) &response_write,        2 },
  { "response_send_error",   (DL_FUNC) &response_send_error,   3 },
  { "response_send_chunk",   (DL_FUNC) &response_send_chunk,   2 },
  /* others */
  { "webfakes_crc32",        (DL_FUNC) &webfakes_crc32,        1 },
  { NULL, NULL, 0 }
};

void R_init_webfakes(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  cleancall_fns_dot_call = Rf_findVar(Rf_install(".Call"), R_BaseEnv);
  /* Once we require some features we need to check the return value. */
  mg_init_library(0);
}

/* --------------------------------------------------------------------- */
/* for older macOS versions                                              */
/* --------------------------------------------------------------------- */

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <sys/time.h>
static int webfakes_clock_gettime(int clk_id, struct timespec *t) {
  memset(t, 0, sizeof(*t));
  if (clk_id == CLOCK_REALTIME) {
    struct timeval now;
    int rv = gettimeofday(&now, NULL);
    if (rv) {
      return rv;
    }
    t->tv_sec = now.tv_sec;
    t->tv_nsec = now.tv_usec * 1000;
    return 0;

  } else if (clk_id == CLOCK_MONOTONIC) {
    static uint64_t clock_start_time = 0;
    static mach_timebase_info_data_t timebase_ifo = {0, 0};

    uint64_t now = mach_absolute_time();

    if (clock_start_time == 0) {
      kern_return_t mach_status = mach_timebase_info(&timebase_ifo);

      /* appease "unused variable" warning for release builds */
      (void)mach_status;

      clock_start_time = now;
    }

    now = (uint64_t)((double)(now - clock_start_time)
                     * (double)timebase_ifo.numer
                     / (double)timebase_ifo.denom);

    t->tv_sec = now / 1000000000;
    t->tv_nsec = now % 1000000000;
    return 0;
  }
  return -1; /* EINVAL - Clock ID is unknown */
}
#else
#define webfakes_clock_gettime(a,b) clock_gettime(a,b)
#endif

/* --------------------------------------------------------------------- */
/* internals                                                             */
/* --------------------------------------------------------------------- */

#ifdef _WIN32
int check_stdin() {
  HANDLE hnd = GetStdHandle(STD_INPUT_HANDLE);
  if (hnd == INVALID_HANDLE_VALUE) {
    R_THROW_SYSTEM_ERROR("Cannot get stdin handle");
  }
  DWORD type = GetFileType(hnd);
  if (type != FILE_TYPE_PIPE) return 0;
  DWORD ret = WaitForSingleObject(hnd, 0);
  if (ret == WAIT_TIMEOUT) return 0;
  if (ret == WAIT_FAILED) R_THROW_SYSTEM_ERROR("Cannot poll stdin");
  if (ret == WAIT_OBJECT_0) {
    DWORD avail;
    DWORD ret2 = PeekNamedPipe(hnd, NULL, 0, NULL, &avail, NULL);
    if (!ret2) {
      DWORD err = GetLastError();
      if (err == ERROR_BROKEN_PIPE) return 1;
      R_THROW_SYSTEM_ERROR_CODE(err, "Cannot peek stdin pipe");
    }
  }
  return 0;
}

#else
int check_stdin() {
  static char buffer[4096];
  struct pollfd pfd;
  pfd.fd = 0;
  pfd.events = POLLIN;
  pfd.revents = 0;
  int ret = poll(&pfd, 1, 0);
  if (ret == -1) R_THROW_SYSTEM_ERROR("Cannot poll stdin");

  // Nothing happened on stdin, keep running
  if (ret == 0) return 0;

  // Event on stdin, if we cannot read, then it if EOF
  ssize_t num = read(0, buffer, 4096);
  if (num == -1) R_THROW_SYSTEM_ERROR("Cannot read from stdin");
  if (num == 0) return 1;

  // Otherwise fine
  return 0;
}
#endif

#define WEBFAKES_NOTHING 0
#define WEBFAKES_REQ     1         /* request just came it */
#define WEBFAKES_WAIT    2         /* waited a bit / wait a bit */
#define WEBFAKES_DONE    3         /* request is done */

struct server_user_data {
  SEXP requests;
  pthread_cond_t process_more;  /* there is something to process */
  pthread_cond_t process_less;  /* we can process something */
  pthread_mutex_t process_lock;
  struct mg_connection *nextconn;
  struct mg_server_port ports[4];
  int num_ports;
  int shutdown;
};

struct connection_user_data {
  pthread_cond_t finish_cond;   /* can finish callback? */
  pthread_mutex_t finish_lock;
  int main_todo;                /* what should the main thread do? */
  int req_todo;                 /* what shoudl the request thread do? */
  double secs;                  /* how much should we wait? */
  SEXP req;
  int id;
};

SEXP webfakes_create_request(struct mg_connection *conn);

#define PTHCHK(expr) if ((ret = expr)) {                                \
  mg_cry(conn, "ERROR @ %s %s:%d", __func__, __FILE__, __LINE__);       \
  R_THROW_SYSTEM_ERROR_CODE(                                            \
    ret, "Cannot process webfakes web server requests");                 \
}

#define CHK(expr) if ((ret = expr) < 0) {                               \
  mg_cry(conn, "ERROR @ %s %s:%d", __func__, __FILE__, __LINE__);       \
  R_THROW_ERROR("Cannot process webfakes web server requests");          \
}

static R_INLINE SEXP new_env() {
  SEXP env;
  PROTECT(env = allocSExp(ENVSXP));
  SET_FRAME(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);
  UNPROTECT(1);
  return env;
}

static void SEXP_to_char_vector(SEXP x, char*** vec) {
  int i, len = LENGTH(x);
  SEXP nms = PROTECT(getAttrib(x, R_NamesSymbol));
  *vec = (char**) R_alloc(2 * len + 1, sizeof(char*));
  for (i = 0; i < len; i++) {
    (*vec)[2 * i    ] = (char*) CHAR(STRING_ELT(nms, i));
    (*vec)[2 * i + 1] = (char*) CHAR(STRING_ELT(x,   i));
  }
  (*vec)[2 * len] = NULL;
  UNPROTECT(1);
}

static int ms_sleep(struct server_user_data* srv_data, int ms) {
  int tosleep = ms > 100 ? 100 : ms;

  while (tosleep > 0) {
#ifdef _WIN32
    Sleep(tosleep);
#else
    usleep(tosleep * 1000);
#endif
    if (srv_data->shutdown) return 1;
    ms -= tosleep;
    tosleep = ms > 100 ? 100 : ms;
  }

  return 0;
}

/* --------------------------------------------------------------------- */
/* civetweb callbacks                                                    */
/* --------------------------------------------------------------------- */

static int begin_request(struct mg_connection *conn) {

#ifndef NDEBUG
  fprintf(stderr, "conn %p: starting\n", conn);
#endif

  struct mg_context *ctx = mg_get_context(conn);
  struct server_user_data *srv_data = mg_get_user_data(ctx);
  if (srv_data->shutdown) return 1;
  struct connection_user_data conn_data = {
    PTHREAD_COND_INITIALIZER, PTHREAD_MUTEX_INITIALIZER,
    WEBFAKES_REQ, WEBFAKES_NOTHING, 0.0, R_NilValue, 0,
  };
  mg_set_user_connection_data(conn, &conn_data);

  if (pthread_mutex_lock(&conn_data.finish_lock)) goto exit;

  while (1) {
    if (pthread_mutex_lock(&srv_data->process_lock)) goto exit;
#ifndef NDEBUG
    fprintf(stderr, "conn %p: waiting for slot\n", conn);
#endif
    while (srv_data->nextconn != NULL) {
      pthread_cond_wait(&srv_data->process_less, &srv_data->process_lock);
    }

#ifndef NDEBUG
    fprintf(stderr, "conn %p: scheduled\n", conn);
#endif
    srv_data->nextconn = conn;  /* only used to pass it to the main thread */

    if (srv_data->shutdown) goto exit;

    if (pthread_cond_signal(&srv_data->process_more)) goto exit;
    if (pthread_mutex_unlock(&srv_data->process_lock)) goto exit;

    /* Need to wait for the response... */
#ifndef NDEBUG
    fprintf(stderr, "conn %p: waiting for order\n", conn);
#endif
    while (conn_data.req_todo == WEBFAKES_NOTHING) {
      if (pthread_cond_wait(&conn_data.finish_cond,
                            &conn_data.finish_lock)) {
        goto exit;
      }
    }
#ifndef NDEBUG
    fprintf(stderr, "conn %p: got order: %d\n", conn, conn_data.req_todo);
#endif
    if (conn_data.req_todo == WEBFAKES_DONE) goto exit;
    if (conn_data.req_todo == WEBFAKES_WAIT) {
#ifndef NDEBUG
      fprintf(stderr, "conn %p: sleeping\n", conn);
#endif

    if (ms_sleep(srv_data, conn_data.secs * 1000)) goto exit;

#ifndef NDEBUG
      fprintf(stderr, "conn %p: sleeping done\n", conn);
#endif
    }
    if (srv_data->shutdown) goto exit;
    conn_data.main_todo = WEBFAKES_WAIT;
    conn_data.req_todo = WEBFAKES_NOTHING;
  }

 exit:
#ifndef NDEBUG
  fprintf(stderr, "conn %p: good bye all\n", conn);
#endif
  mg_set_user_connection_data(conn, NULL);
  pthread_mutex_unlock(&conn_data.finish_lock);
  pthread_mutex_destroy(&conn_data.finish_lock);
  pthread_cond_destroy(&conn_data.finish_cond);

  return 1;
}

/* --------------------------------------------------------------------- */
/* server                                                                */
/* --------------------------------------------------------------------- */

static int register_request(struct server_user_data *srv_data, SEXP req) {
  SEXP nextid = PROTECT(Rf_install("nextid"));
  int id = INTEGER(Rf_findVar(nextid, srv_data->requests))[0] + 1;
  SEXP xid = PROTECT(ScalarInteger(id));
  Rf_defineVar(nextid, xid, srv_data->requests);
  SEXP cid = PROTECT(asChar(xid));
  SEXP rname = PROTECT(Rf_installChar(cid));
  Rf_defineVar(rname, req, srv_data->requests);
  UNPROTECT(4);
  return id;
}

static void deregister_request(struct server_user_data *srv_data, int id) {
  SEXP xid = PROTECT(ScalarInteger(id));
  SEXP cid = PROTECT(asChar(xid));
  SEXP rname = PROTECT(Rf_installChar(cid));
  Rf_defineVar(rname, R_NilValue, srv_data->requests);
  UNPROTECT(3);
}

static void release_all_requests(SEXP requests) {
  SEXP nms = PROTECT(R_lsInternal3(requests, 1, 0));
  int i, n = LENGTH(nms);
  for (i = 0; i < n; i++) {
    const char *nm = CHAR(STRING_ELT(nms, i));
    if (!strcmp("nextid", nm)) continue;
    SEXP sym = PROTECT(Rf_installChar(STRING_ELT(nms, i)));
    SEXP req = Rf_findVar(sym, requests);
    if (!isNull(req)) {
      SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
      struct mg_connection *conn = R_ExternalPtrAddr(xconn);
      if (conn) {
#ifndef NDEBUG
        fprintf(stderr, "conn %p: emergency cleanup\n", conn);
#endif
        struct connection_user_data *conn_data = mg_get_user_connection_data(conn);
        struct mg_context *ctx = mg_get_context(conn);
        struct server_user_data *srv_data = mg_get_user_data(ctx);
        pthread_mutex_lock(&conn_data->finish_lock);
        conn_data->req_todo = WEBFAKES_DONE;
        conn_data->req = R_NilValue;
        pthread_cond_signal(&conn_data->finish_cond);
        pthread_mutex_unlock(&conn_data->finish_lock);
        pthread_cond_signal(&srv_data->process_less);
      }
    }
    UNPROTECT(1);
  }

  UNPROTECT(1);
}

static void webfakes_server_finalizer(SEXP server) {
  struct mg_context *ctx = R_ExternalPtrAddr(server);
  if (ctx == NULL) return;
#ifndef NDEBUG
  fprintf(stderr, "serv %p: cleaning up\n", ctx);
#endif
  R_ClearExternalPtr(server);
  struct server_user_data* srv_data = mg_get_user_data(ctx);
  srv_data->shutdown = 1;
  release_all_requests(srv_data->requests);
#ifndef NDEBUG
  fprintf(stderr, "serv %p: waiting for worker threads\n", ctx);
  void *ctx_addr = ctx;
#endif
  mg_stop(ctx);
  int ret = 0;
  ret += pthread_mutex_unlock(&srv_data->process_lock);
  ret += pthread_mutex_destroy(&srv_data->process_lock);
  ret += pthread_cond_destroy(&srv_data->process_more);
  ret += pthread_cond_destroy(&srv_data->process_less);
  free(srv_data);
#ifndef NDEBUG
  fprintf(stderr, "serv %p: that would be all for today\n", ctx_addr);
#endif
}

SEXP server_start(SEXP options) {

#ifndef NDEBUG
  fprintf(stderr, "creating new server\n");
#endif

  SEXP server = R_NilValue;
  struct server_user_data *srv_data =
    malloc(sizeof(struct server_user_data));
  if (!srv_data) R_THROW_SYSTEM_ERROR("Cannot start webfakes server");
  struct mg_context *ctx = NULL;
  int ret = 0;

  memset(srv_data, 0, sizeof(struct server_user_data));

  srv_data->requests = PROTECT(new_env());
  SEXP x1 = PROTECT(ScalarInteger(1));
  Rf_defineVar(Rf_install("nextid"), x1, srv_data->requests);
  UNPROTECT(1);
  if ((ret = pthread_cond_init(&srv_data->process_more, NULL))) goto cleanup;
  if ((ret = pthread_cond_init(&srv_data->process_less, NULL))) goto cleanup;
  if ((ret = pthread_mutex_init(&srv_data->process_lock, NULL))) goto cleanup;

  char **coptions;
  SEXP_to_char_vector(options, &coptions);
  struct mg_callbacks callbacks;

  memset(&callbacks, 0, sizeof(callbacks));
  callbacks.begin_request = begin_request;

  if ((ret = pthread_mutex_lock(&srv_data->process_lock))) goto cleanup;
  ctx = mg_start(&callbacks, srv_data, (const char **) coptions);
  if (ctx == NULL) goto cleanup;
  PROTECT(server = R_MakeExternalPtr(ctx, srv_data->requests, R_NilValue));
  R_RegisterCFinalizer(server, webfakes_server_finalizer);

#ifndef NDEBUG
  fprintf(stderr, "serv %p: hi everyone, ready to serve\n", ctx);
#endif

  memset(srv_data->ports, 0, sizeof(srv_data->ports));
  srv_data->num_ports = mg_get_server_ports(
    ctx,
    sizeof(srv_data->ports) / sizeof(struct mg_server_port),
    srv_data->ports
  );
  if (srv_data->num_ports < 0) goto cleanup;

  UNPROTECT(2);
  return server;

 cleanup:
#ifndef NDEBUG
  fprintf(stderr, "serv %p: failed to start new server\n", ctx);
#endif
  /* This is unlocked in the finalizer, but that might be much later... */
  if (ctx) mg_stop(ctx);
  pthread_mutex_unlock(&srv_data->process_lock);
  if (ret) {
    R_THROW_SYSTEM_ERROR_CODE(ret, "Cannot start webfakes web server");
  } else {
    R_THROW_ERROR("Cannot start webfakes web server");
  }

  /* Never reached */
  return R_NilValue;
}

static void server_poll_cleanup(void *ptr) {
#ifndef NDEBUG
  fprintf(stderr, "conn %p: oh-oh, forced cleanup\n", ptr);
#endif
  struct mg_connection *conn = (struct mg_connection*) ptr;
  struct connection_user_data *conn_data = mg_get_user_connection_data(conn);
  struct mg_context *ctx = mg_get_context(conn);
  struct server_user_data *srv_data = mg_get_user_data(ctx);
  mg_cry(conn, "Cleaning up broken connection at %s:%d", __FILE__, __LINE__);
  pthread_mutex_lock(&conn_data->finish_lock);
  conn_data->req_todo = WEBFAKES_DONE;
  deregister_request(srv_data, conn_data->id);
  conn_data->req = R_NilValue;
  pthread_cond_signal(&conn_data->finish_cond);
  pthread_mutex_unlock(&conn_data->finish_lock);
  pthread_cond_signal(&srv_data->process_less);
}

SEXP server_poll(SEXP server, SEXP clean) {
  struct mg_context *ctx = R_ExternalPtrAddr(server);
  int cclean = LOGICAL(clean)[0];
#ifndef NDEBUG
  fprintf(stderr, "serv %p: polling\n", ctx);
#endif
  if (ctx == NULL) R_THROW_ERROR("webfakes server has stopped already");
  struct server_user_data *srv_data = mg_get_user_data(ctx);

  struct timespec limit;
  while (srv_data->nextconn == NULL) {
    webfakes_clock_gettime(CLOCK_REALTIME, &limit);
    limit.tv_nsec += 50 * 1000 * 1000;
    if (limit.tv_nsec >= 1000 * 1000 * 1000) {
      limit.tv_sec += 1;
      limit.tv_nsec %= 1000 * 1000 * 1000;
    }
    R_CheckUserInterrupt();
    if (cclean && check_stdin()) R_THROW_ERROR("Cleaning up web server");
    (void) pthread_cond_timedwait(&srv_data->process_more,
                                  &srv_data->process_lock, &limit);
  }

  struct mg_connection *conn = srv_data->nextconn;
  srv_data->nextconn = NULL;
  struct connection_user_data *conn_data = mg_get_user_connection_data(conn);

#ifndef NDEBUG
  fprintf(stderr, "serv %p: processing conn %p\n", ctx, conn);
#endif

  SEXP req = R_NilValue;
  switch(conn_data->main_todo) {
  case WEBFAKES_REQ:
    r_call_on_early_exit(server_poll_cleanup, conn);
    req = webfakes_create_request(conn);
    break;
  case WEBFAKES_WAIT:
    req = conn_data->req;
    break;
  default:
    break;
  }

#ifndef NDEBUG
  fprintf(stderr, "serv %p: returning request from conn %p\n", ctx, conn);
#endif

  return req;
}

SEXP server_stop(SEXP server) {
  webfakes_server_finalizer(server);
  return R_NilValue;
}

SEXP server_get_ports(SEXP server) {
  struct mg_context *ctx = R_ExternalPtrAddr(server);
  if (ctx == NULL) R_THROW_ERROR("webfakes server has stopped already");
  struct server_user_data *srv_data = mg_get_user_data(ctx);

  int i, num_ports = srv_data->num_ports;
  SEXP ipv4 = PROTECT(allocVector(LGLSXP, num_ports));
  SEXP ipv6 = PROTECT(allocVector(LGLSXP, num_ports));
  SEXP port = PROTECT(allocVector(INTSXP, num_ports));
  SEXP ssl  = PROTECT(allocVector(LGLSXP, num_ports));

  const char *res_names[] = { "ipv4", "ipv6", "port", "ssl", "" };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, res_names));
  for (i = 0; i < num_ports; i++) {
    LOGICAL(ipv4)[i] = (srv_data->ports[i].protocol) & 1;
    LOGICAL(ipv6)[i] = (srv_data->ports[i].protocol) & 2;
    INTEGER(port)[i] = srv_data->ports[i].port;
    LOGICAL(ssl )[i] = srv_data->ports[i].is_ssl == 1;
  }

  SET_VECTOR_ELT(res, 0, ipv4);
  SET_VECTOR_ELT(res, 1, ipv6);
  SET_VECTOR_ELT(res, 2, port);
  SET_VECTOR_ELT(res, 3, ssl);

  UNPROTECT(5);
  return res;
}

/* --------------------------------------------------------------------- */
/* request                                                               */
/* --------------------------------------------------------------------- */

SEXP webfakes_create_request(struct mg_connection *conn) {
  static char request_link[8192];
  int i;
  SEXP x;

#ifndef NDEBUG
  fprintf(stderr, "conn %p: creating an R request object\n", conn);
#endif

  const struct mg_request_info *req_info = mg_get_request_info(conn);
  SEXP req = PROTECT(new_env());

  x = PROTECT(mkString(req_info->request_method));
  defineVar(Rf_install("method"), x, req);
  UNPROTECT(1);

  mg_get_request_link(conn, request_link, sizeof(request_link));
  x = PROTECT(mkString(request_link));
  defineVar(Rf_install("url"), x, req);
  UNPROTECT(1);

  x = PROTECT(mkString(req_info->request_uri));
  defineVar(Rf_install("request_uri"), x, req);
  UNPROTECT(1);

  x = PROTECT(mkString(req_info->local_uri));
  defineVar(Rf_install("path"), x, req);
  UNPROTECT(1);

  x = PROTECT(mkString(req_info->http_version));
  defineVar(Rf_install("http_version"), x, req);
  UNPROTECT(1);

  x = PROTECT(req_info->query_string ? mkString(req_info->query_string) :
              mkString(""));
  defineVar(Rf_install("query_string"), x, req);
  UNPROTECT(1);

  x = PROTECT(mkString(req_info->remote_addr));
  defineVar(Rf_install("remote_addr"), x, req);
  UNPROTECT(1);

  x = PROTECT(ScalarReal(req_info->content_length));
  defineVar(Rf_install("content_length"), x, req);
  UNPROTECT(1);

  x = PROTECT(ScalarInteger(req_info->remote_port));
  defineVar(Rf_install("remote_port"), x, req);
  UNPROTECT(1);

  SEXP hdr = PROTECT(allocVector(VECSXP, req_info->num_headers));
  SEXP nms = PROTECT(allocVector(STRSXP, req_info->num_headers));
  for (i = 0; i < req_info->num_headers; i++) {
      SET_VECTOR_ELT(hdr, i, mkString(req_info->http_headers[i].value));
      SET_STRING_ELT(nms, i, mkChar(req_info->http_headers[i].name));
  }
  Rf_setAttrib(hdr, R_NamesSymbol, nms);
  defineVar(Rf_install("headers"), hdr, req);

  if (req_info->content_length != -1) {
    SEXP body = PROTECT(allocVector(RAWSXP, req_info->content_length));
    int ret = mg_read(conn, RAW(body), req_info->content_length);
    if (ret < 0) {
      mg_cry(conn, "ERROR @ %s %s:%d", __func__, __FILE__, __LINE__);
      R_THROW_ERROR("Cannot read from webfakes HTTP client");
    }
    if (ret != req_info->content_length) {
      warning("Partial HTTP request body from client");
    }
    defineVar(Rf_install(".body"), body, req);
    UNPROTECT(1);
  } else {
    defineVar(Rf_install(".body"), R_NilValue, req);
  }

  SEXP xreq = PROTECT(R_MakeExternalPtr(conn, R_NilValue, R_NilValue));
  defineVar(Rf_install(".xconn"), xreq, req);
  UNPROTECT(1);

  struct connection_user_data *conn_data = mg_get_user_connection_data(conn);
  conn_data->req = req;
  struct mg_context *ctx = mg_get_context(conn);
  struct server_user_data *srv_data = mg_get_user_data(ctx);
  conn_data->id = register_request(srv_data, req);

  UNPROTECT(3);
  return req;
}

static void response_cleanup(void *ptr) {
  struct mg_connection *conn = (struct mg_connection*) ptr;
  struct connection_user_data *conn_data = mg_get_user_connection_data(conn);
  struct mg_context *ctx = mg_get_context(conn);
  struct server_user_data *srv_data = mg_get_user_data(ctx);
  if (conn_data) {
#ifndef NDEBUG
    fprintf(stderr, "conn %p: oh-oh, emergency cleanup\n", ptr);
#endif
    mg_set_user_connection_data(conn, NULL);
    mg_cry(conn, "Cleaning up broken connection %p at %s:%d", conn,
           __FILE__, __LINE__);
    pthread_mutex_lock(&conn_data->finish_lock);
    conn_data->req_todo = WEBFAKES_DONE;
    deregister_request(srv_data, conn_data->id);
    SEXP req = conn_data->req;
    SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
    R_ClearExternalPtr(xconn);
    conn_data->req = R_NilValue;
    pthread_cond_signal(&conn_data->finish_cond);
    pthread_mutex_unlock(&conn_data->finish_lock);
  }
  pthread_cond_signal(&srv_data->process_less);
}

SEXP response_delay(SEXP req, SEXP secs) {
  SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
  struct mg_connection *conn = R_ExternalPtrAddr(xconn);
  if (conn == 0) {
#ifndef NDEBUG
    fprintf(stderr, "?? a connection was cleaned up already\n");
    return R_NilValue;
#endif
  }
  struct mg_context *ctx = mg_get_context(conn);
#ifndef NDEBUG
  fprintf(stderr, "serv %p: telling conn %p to delay\n", ctx, conn);
#endif
  struct connection_user_data *conn_data = mg_get_user_connection_data(conn);
  int ret;

  r_call_on_early_exit(response_cleanup, conn);

  pthread_mutex_lock(&conn_data->finish_lock);
  conn_data->secs = REAL(secs)[0];
  conn_data->req_todo = WEBFAKES_WAIT;

  PTHCHK(pthread_cond_signal(&conn_data->finish_cond));
  PTHCHK(pthread_mutex_unlock(&conn_data->finish_lock));

  struct server_user_data *srv_data = mg_get_user_data(ctx);

#ifndef NDEBUG
  fprintf(stderr, "serv %p: inviting request threads\n", ctx);
#endif

  PTHCHK(pthread_cond_signal(&srv_data->process_less));

  return R_NilValue;
}

SEXP response_send_headers(SEXP req) {
  SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
  struct mg_connection *conn = R_ExternalPtrAddr(xconn);
  if (conn == 0) {
#ifndef NDEBUG
    fprintf(stderr, "?? a connection was cleaned up already\n");
    return R_NilValue;
#endif
  }
#ifndef NDEBUG
  fprintf(stderr, "conn %p: sending response headers\n", conn);
#endif

  r_call_on_early_exit(response_cleanup, conn);

  SEXP http_version = PROTECT(Rf_findVar(Rf_install("http_version"), req));
  SEXP res = PROTECT(Rf_findVar(Rf_install("res"), req));
  SEXP headers = PROTECT(Rf_findVar(Rf_install(".headers"), res));
  SEXP names = PROTECT(Rf_getAttrib(headers, R_NamesSymbol));
  SEXP status = PROTECT(Rf_findVar(Rf_install(".status"), res));
  int ret, i, nh = isNull(headers) ? 0 : LENGTH(headers);

  CHK(mg_printf(conn, "HTTP/%s %d %s\r\n", CHAR(STRING_ELT(http_version, 0)),
                INTEGER(status)[0], mg_get_response_code_text(conn, INTEGER(status)[0])));

  for (i = 0; i < nh; i++) {
    const char *k = CHAR(STRING_ELT(names, i));
    const char *v = CHAR(STRING_ELT(VECTOR_ELT(headers, i), 0));
    CHK(mg_printf(conn, "%s: %s\r\n", k, v));
  }
  CHK(mg_printf(conn, "\r\n"));

#ifndef NDEBUG
  fprintf(stderr, "conn %p: response headers sent\n", conn);
#endif

  UNPROTECT(5);
  return R_NilValue;
}

SEXP response_send(SEXP req) {
  SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
  struct mg_connection *conn = R_ExternalPtrAddr(xconn);
  if (conn == 0) {
#ifndef NDEBUG
    fprintf(stderr, "?? a connection was cleaned up already\n");
    return R_NilValue;
#endif
  }
#ifndef NDEBUG
  fprintf(stderr, "conn %p: sending response body\n", conn);
#endif
  SEXP res = PROTECT(Rf_findVar(Rf_install("res"), req));
  SEXP headers_sent = Rf_findVar(Rf_install("headers_sent"), res);
  if (! LOGICAL(headers_sent)[0]) response_send_headers(req);

  struct connection_user_data *conn_data = mg_get_user_connection_data(conn);
  r_call_on_early_exit(response_cleanup, conn);
  SEXP body = Rf_findVar(Rf_install(".body"), res);
  int ret;

  if (TYPEOF(body) == RAWSXP) {
    CHK(mg_write(conn, RAW(body), LENGTH(body)));
  } else if (TYPEOF(body) == STRSXP) {
    const char *cbody = CHAR(STRING_ELT(body, 0));
    CHK(mg_write(conn, cbody, strlen(cbody)));
  }

  struct mg_context *ctx = mg_get_context(conn);
  struct server_user_data *srv_data = mg_get_user_data(ctx);

#ifndef NDEBUG
  fprintf(stderr, "conn %p: response body sent\n", conn);
#endif

  pthread_mutex_lock(&conn_data->finish_lock);

  conn_data->req_todo = WEBFAKES_DONE;
  deregister_request(srv_data, conn_data->id);
  conn_data->req = R_NilValue;

#ifndef NDEBUG
  fprintf(stderr, "serv %p: telling conn %p to quit\n", ctx, conn);
#endif

  PTHCHK(pthread_cond_signal(&conn_data->finish_cond));
  PTHCHK(pthread_mutex_unlock(&conn_data->finish_lock));

#ifndef NDEBUG
  fprintf(stderr, "serv %p: inviting request threads\n", ctx);
#endif

  PTHCHK(pthread_cond_signal(&srv_data->process_less));

  UNPROTECT(1);
  return R_NilValue;
}

SEXP response_write(SEXP req, SEXP data) {
  SEXP res = PROTECT(Rf_findVar(Rf_install("res"), req));
  SEXP headers_sent = PROTECT(Rf_findVar(Rf_install("headers_sent"), res));
  if (! LOGICAL(headers_sent)[0]) response_send_headers(req);

  SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
  struct mg_connection *conn = R_ExternalPtrAddr(xconn);
  if (conn == 0) {
#ifndef NDEBUG
    fprintf(stderr, "?? a connection was cleaned up already\n");
    return R_NilValue;
#endif
  }

  r_call_on_early_exit(response_cleanup, conn);

  int ret = 0;
  int len = LENGTH(data);

#ifndef NDEBUG
  fprintf(stderr, "conn %p: writing %d bytes\n", conn, len);
#endif
  CHK(mg_write(conn, RAW(data), len));

  UNPROTECT(2);
  return R_NilValue;
}

SEXP response_send_chunk(SEXP req, SEXP data) {
  SEXP res = PROTECT(Rf_findVar(Rf_install("res"), req));
  SEXP headers_sent = PROTECT(Rf_findVar(Rf_install("headers_sent"), res));
  if (! LOGICAL(headers_sent)[0]) response_send_headers(req);

  SEXP xconn = Rf_findVar(Rf_install(".xconn"), req);
  struct mg_connection *conn = R_ExternalPtrAddr(xconn);
  if (conn == 0) {
#ifndef NDEBUG
    fprintf(stderr, "?? a connection was cleaned up already\n");
    return R_NilValue;
#endif
  }

  r_call_on_early_exit(response_cleanup, conn);

  int ret = 0;
  int len = LENGTH(data);

#ifndef NDEBUG
  fprintf(stderr, "conn %p: sending chunk of %d bytes\n", conn, len);
#endif
  CHK(mg_send_chunk(conn, (const char*) RAW(data), len));

  UNPROTECT(2);
  return R_NilValue;
}

SEXP response_send_error(SEXP req, SEXP message, SEXP status) {
#ifndef NDEBUG
  fprintf(stderr, "sending 500 response");
#endif
  SEXP res = PROTECT(Rf_findVar(Rf_install("res"), req));
  Rf_defineVar(Rf_install(".body"), message, res);
  Rf_defineVar(Rf_install(".status"), status, res);
  UNPROTECT(1);
  return response_send(req);
}
