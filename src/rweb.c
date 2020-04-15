
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include "civetweb.h"
#include "errors.h"

#include <pthread.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

SEXP server_start(SEXP options);
SEXP server_process(SEXP rsrv, SEXP handler, SEXP env);
SEXP server_stop(SEXP rsrv);
SEXP server_get_ports(SEXP rsrv);

SEXP presser_crc32(SEXP v);

static const R_CallMethodDef callMethods[]  = {
  { "server_start",     (DL_FUNC) &server_start,     1 },
  { "server_process",   (DL_FUNC) &server_process,   3 },
  { "server_stop",      (DL_FUNC) &server_stop,      1 },
  { "server_get_ports", (DL_FUNC) &server_get_ports, 1 },
  { "presser_crc32",   (DL_FUNC) &presser_crc32,    1 },
  { NULL, NULL, 0 }
};

void R_init_presser(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  /* Once we require some features we need to check the return value. */
  mg_init_library(0);
}

#define PRESSER_NOTHING 0
#define PRESSER_REQ     1         /* request just came it */
#define PRESSER_WAIT    2         /* waited a bit / wait a bit */
#define PRESSER_DONE    3         /* request is done */

struct presser_server {
  struct mg_context *ctx;
  pthread_cond_t process_more;  /* there is something to process */
  pthread_cond_t process_less;  /* we can process something */
  pthread_mutex_t process_lock;
  struct mg_connection *conn;   /* the currenty active connection or NULL */
  struct mg_server_port ports[4];
  int num_ports;
};

struct presser_connection {
  pthread_cond_t finish_cond;   /* can finish callback? */
  pthread_mutex_t finish_lock;
  int main_todo;                /* what should the main thread do? */
  int req_todo;                 /* what shoudl the request thread do? */
  double secs;                  /* how much should we wait? */
  SEXP req;
};

void SEXP_to_char_vector(SEXP x, char*** vec) {
  int i, len = LENGTH(x);
  SEXP nms = getAttrib(x, R_NamesSymbol);
  *vec = (char**) R_alloc(2 * len + 1, sizeof(char*));
  for (i = 0; i < len; i++) {
    (*vec)[2 * i    ] = (char*) CHAR(STRING_ELT(nms, i));
    (*vec)[2 * i + 1] = (char*) CHAR(STRING_ELT(x,   i));
  }
  (*vec)[2 * len] = NULL;
}

static int begin_request(struct mg_connection *conn) {

  struct mg_context *ctx = mg_get_context(conn);
  struct presser_server *srv = mg_get_user_data(ctx);
  struct presser_connection conn_data = {
    PTHREAD_COND_INITIALIZER, PTHREAD_MUTEX_INITIALIZER,
    PRESSER_REQ, PRESSER_NOTHING, 0.0, R_NilValue
  };

  if (pthread_mutex_lock(&conn_data.finish_lock)) goto exit;

  while (1) {
    if (pthread_mutex_lock(&srv->process_lock)) goto exit;
    mg_set_user_connection_data(conn, &conn_data);
    while (srv->conn != NULL) {
      pthread_cond_wait(&srv->process_less, &srv->process_lock);
    }

    srv->conn = conn;

    if (pthread_cond_signal(&srv->process_more)) goto exit;
    if (pthread_mutex_unlock(&srv->process_lock)) goto exit;

    /* Need to wait for the response... */
    while (conn_data.req_todo == PRESSER_NOTHING) {
      if (pthread_cond_wait(&conn_data.finish_cond,
                            &conn_data.finish_lock)) {
        goto exit;
      }
    }
    if (conn_data.req_todo == PRESSER_DONE) break;
    if (conn_data.req_todo == PRESSER_WAIT) {
#ifdef _WIN32
      Sleep(conn_data.secs * 1000);
#else
      usleep(conn_data.secs * 1000 * 1000);
#endif
    }
    conn_data.main_todo = PRESSER_WAIT;
    conn_data.req_todo = PRESSER_NOTHING;
  }

 exit:
  pthread_mutex_unlock(&conn_data.finish_lock);
  mg_set_user_connection_data(conn, NULL);

  return 1;
}

static int init_connection(const struct mg_connection *conn,
                           void **conn_data) {
  *conn_data = NULL;
  return 0;
}

static void connection_close(const struct mg_connection *conn) {
  struct presser_connection *conn_data = mg_get_user_connection_data(conn);
  if (conn_data == NULL) return;
  pthread_cond_destroy(&conn_data->finish_cond);
  pthread_mutex_unlock(&conn_data->finish_lock);
  pthread_mutex_destroy(&conn_data->finish_lock);
  mg_set_user_connection_data((struct mg_connection*) conn, NULL);
}

static void end_request(const struct mg_connection *conn, int reply_status_code) {
  /* Right now, these two are the same, they are probably redundant,
     but it does not hurt */
  connection_close(conn);
}

static void presser_server_finalizer(SEXP rsrv) {
  /* TODO: what if a thread is waiting on one of these right now? */
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) return;
  int ret = 0;
  R_ClearExternalPtr(rsrv);
  mg_stop(srv->ctx);
  ret += pthread_mutex_unlock(&srv->process_lock);
  ret += pthread_mutex_destroy(&srv->process_lock);
  ret += pthread_cond_destroy(&srv->process_more);
  ret += pthread_cond_destroy(&srv->process_less);
}

SEXP server_start(SEXP options) {

  SEXP rsrv = R_NilValue;
  struct presser_server *srv = malloc(sizeof(struct presser_server));
  if (!srv) R_THROW_SYSTEM_ERROR("Cannot start presser server");
  int ret = 0;

  memset(srv, 0, sizeof(struct presser_server));
  PROTECT(rsrv = R_MakeExternalPtr(srv, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(rsrv, presser_server_finalizer);

  if ((ret = pthread_cond_init(&srv->process_more, NULL))) goto cleanup;
  if ((ret = pthread_cond_init(&srv->process_less, NULL))) goto cleanup;
  if ((ret = pthread_mutex_init(&srv->process_lock, NULL))) goto cleanup;

  char **coptions;
  SEXP_to_char_vector(options, &coptions);
  struct mg_callbacks callbacks;

  memset(&callbacks, 0, sizeof(callbacks));
  callbacks.begin_request = begin_request;
  callbacks.end_request = end_request;
  callbacks.init_connection = init_connection;
  callbacks.connection_close = connection_close;

  if ((ret = pthread_mutex_lock(&srv->process_lock))) goto cleanup;
  srv->ctx = mg_start(&callbacks, srv, (const char **) coptions);
  if (srv->ctx == NULL) goto cleanup;

  memset(srv->ports, 0, sizeof(srv->ports));
  srv->num_ports = mg_get_server_ports(
    srv->ctx,
    sizeof(srv->ports) / sizeof(struct mg_server_port),
    srv->ports
  );
  if (srv->num_ports < 0) goto cleanup;

  UNPROTECT(1);
  return rsrv;

 cleanup:
  /* This is unlocked in the finalizer, but that might be much later... */
  if (srv->ctx) mg_stop(srv->ctx);
  pthread_mutex_unlock(&srv->process_lock);
  if (ret) {
    R_THROW_SYSTEM_ERROR_CODE(ret, "Cannot start presser web server");
  } else {
    R_THROW_ERROR("Cannot start presser web server");
  }

  /* Never reached */
  return R_NilValue;
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

#define CHK(expr) if ((ret = expr))                                     \
    R_THROW_SYSTEM_ERROR_CODE(ret, "Cannot process presser web server requests")

SEXP presser_create_request(SEXP rsrv) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) R_THROW_ERROR("presser server has stopped already");
  static char request_link[8192];
  int i;

  /* Actual request processing */

  const struct mg_request_info *req = mg_get_request_info(srv->conn);
  SEXP rreq = PROTECT(new_env());
  defineVar(install("method"), mkString(req->request_method), rreq);
  mg_get_request_link(srv->conn, request_link, sizeof(request_link));
  defineVar(install("url"), mkString(request_link), rreq);
  defineVar(install("request_uri"), mkString(req->request_uri), rreq);
  defineVar(install("path"), mkString(req->local_uri), rreq);
  defineVar(install("http_version"), mkString(req->http_version), rreq);
  defineVar(
    install("query_string"),
    req->query_string ? mkString(req->query_string) : mkString(""),
    rreq
  );
  defineVar(install("remote_addr"), mkString(req->remote_addr), rreq);
  defineVar(install("content_length"), ScalarReal(req->content_length), rreq);
  defineVar(install("remote_port"), ScalarInteger(req->remote_port), rreq);

  SEXP hdr = PROTECT(allocVector(VECSXP, req->num_headers));
  SEXP nms = PROTECT(allocVector(STRSXP, req->num_headers));
  for (i = 0; i < req->num_headers; i++) {
      SET_VECTOR_ELT(hdr, i, mkString(req->http_headers[i].value));
      SET_STRING_ELT(nms, i, mkChar(req->http_headers[i].name));
  }
  Rf_setAttrib(hdr, R_NamesSymbol, nms);
  defineVar(install("headers"), hdr, rreq);

  if (req->content_length != -1) {
    SEXP body = PROTECT(allocVector(RAWSXP, req->content_length));
    int ret = mg_read(srv->conn, RAW(body), req->content_length);
    if (ret < 0) R_THROW_ERROR("Cannot read from presser HTTP client");
    if (ret != req->content_length) {
      warning("Partial HTTP request body from client");
    }
    defineVar(install(".body"), body, rreq);
    UNPROTECT(1);
  } else {
    defineVar(install(".body"), R_NilValue, rreq);
  }

  UNPROTECT(3);
  return rreq;
}

void presser_send_response(SEXP res, SEXP rsrv) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) R_THROW_ERROR("presser server has stopped already");
  int ret, i;

  const struct mg_request_info *rreq = mg_get_request_info(srv->conn);

  if (TYPEOF(res) != STRSXP) res = VECTOR_ELT(res, 1);

  if (TYPEOF(res) == STRSXP && LENGTH(res) > 0) {
    if (LENGTH(res) > 1) {
      warning("Only first element of character vector is used for HTTP body");
    }
    const char *s = CHAR(STRING_ELT(res, 0));
    int len = strlen(s);
    ret = mg_printf(
      srv->conn,
      "HTTP/%s 500 Internal Server Error\r\n"
      "Content-Length: %d\r\n"
      "Content-Type: text/plain\r\n\r\n",
      rreq->http_version, len
    );
    if (ret < 0) R_THROW_ERROR("Could not send HTTP error response");
    if (mg_write(srv->conn, s, len) < 0) {
      R_THROW_ERROR("Failed to write HTTP response body");
    }

  } else if (TYPEOF(res) == VECSXP && LENGTH(res) == 3) {
    SEXP cnt = VECTOR_ELT(res, 0);
    SEXP hdr = VECTOR_ELT(res, 1);
    int code = INTEGER(VECTOR_ELT(res, 2))[0];

    ret = mg_printf(
      srv->conn,
      "HTTP/%s %d %s\r\n",
      rreq->http_version,
      code, mg_get_response_code_text(srv->conn, code)
    );
    if (ret < 0) R_THROW_ERROR("Could not send HTTP response");

    for (i = 0; !isNull(hdr) && i < LENGTH(hdr); i++) {
      const char *hs = CHAR(STRING_ELT(hdr, i));
      ret = mg_write(srv->conn, hs, strlen(hs));
      ret |= mg_write(srv->conn, "\r\n", 2);
      if (ret < 0) R_THROW_ERROR("Could not send HTTP response");
    }

    if (mg_write(srv->conn, "\r\n", 2) < 0) {
      R_THROW_ERROR("Could not send HTTP response");
    }

    if (TYPEOF(cnt) == RAWSXP) {
      ret = mg_write(srv->conn, RAW(cnt), LENGTH(cnt));
    } else if (TYPEOF(cnt) == STRSXP) {
      const char *ccnt = CHAR(STRING_ELT(cnt, 0));
      ret = mg_write(srv->conn, ccnt, strlen(ccnt));
    }
    if (ret < 0) R_THROW_ERROR("Could not send HTTP response");

  } else if (isNull(res)) {
    /* Do nothing. Response is sent or empty response */

  } else {
    R_THROW_ERROR("Invalid presser response");
  }
}

SEXP server_process(SEXP rsrv, SEXP handler, SEXP env) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) R_THROW_ERROR("presser server has stopped already");
  int ret;

  while (1) {
    struct timespec limit;
    while (srv->conn == NULL) {
      clock_gettime(CLOCK_REALTIME, &limit);
      limit.tv_nsec += 50 * 1000 * 1000;
      if (limit.tv_nsec >= 1000 * 1000 * 1000) {
        limit.tv_sec += 1;
        limit.tv_nsec %= 1000 * 1000 * 1000;
      }
      R_CheckUserInterrupt();
      /* TODO: wake up handler callback to avoid a locked server */
      ret = pthread_cond_timedwait(&srv->process_more, &srv->process_lock, &limit);
    }

    struct presser_connection *conn_data =
      mg_get_user_connection_data(srv->conn);

    SEXP req = R_NilValue;
    switch(conn_data->main_todo) {
    case PRESSER_REQ:
      req = PROTECT(presser_create_request(rsrv));
      conn_data->req = req;
      R_PreserveObject(req);
      UNPROTECT(1);
      break;
    case PRESSER_WAIT:
      req = conn_data->req;
      break;
    default:
      break;
    }

    SEXP try = PROTECT(install("try"));
    SEXP silent = PROTECT(ScalarLogical(1));
    SEXP call = PROTECT(lang2(handler, req));
    SEXP trycall = PROTECT(lang3(try, call, silent));
    SEXP res = PROTECT(eval(trycall, env));

    CHK(pthread_mutex_lock(&conn_data->finish_lock));

    /* TODO: need to catch errors here, because they do not
       wake up the request thread, and the server freezes. */
    if (TYPEOF(res) != VECSXP || INTEGER(VECTOR_ELT(res, 0))[0] == 0) {
      presser_send_response(res, rsrv);
      conn_data->req_todo = PRESSER_DONE;
      R_ReleaseObject(conn_data->req);
      conn_data->req = R_NilValue;
    } else if (INTEGER(VECTOR_ELT(res, 0))[0] == 1) {
      conn_data->secs = REAL(VECTOR_ELT(res, 1))[0];
      conn_data->req_todo = PRESSER_WAIT;
    } else {
      R_THROW_ERROR("Invalid presser response, internal error");
    }

    /* OK, we are done */
    srv->conn = NULL;

    /* Notify the worker thread */
    CHK(pthread_cond_signal(&conn_data->finish_cond));
    CHK(pthread_mutex_unlock(&conn_data->finish_lock));

    /* Notify other workers */
    pthread_cond_signal(&srv->process_less);
  }

  /* Never returns... */
  UNPROTECT(5);
  return R_NilValue;
}

SEXP server_stop(SEXP rsrv) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv != NULL) presser_server_finalizer(rsrv);
  return R_NilValue;
}

SEXP server_get_ports(SEXP rsrv) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) R_THROW_ERROR("presser server has stopped already");

  int i, num_ports = srv->num_ports;
  SEXP ipv4 = PROTECT(allocVector(LGLSXP, num_ports));
  SEXP ipv6 = PROTECT(allocVector(LGLSXP, num_ports));
  SEXP port = PROTECT(allocVector(INTSXP, num_ports));
  SEXP ssl  = PROTECT(allocVector(LGLSXP, num_ports));

  const char *res_names[] = { "ipv4", "ipv6", "port", "ssl", "" };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, res_names));
  for (i = 0; i < num_ports; i++) {
    LOGICAL(ipv4)[i] = (srv->ports[i].protocol) & 1;
    LOGICAL(ipv6)[i] = (srv->ports[i].protocol) & 2;
    INTEGER(port)[i] = srv->ports[i].port;
    LOGICAL(ssl )[i] = srv->ports[i].is_ssl == 1;
  }

  SET_VECTOR_ELT(res, 0, ipv4);
  SET_VECTOR_ELT(res, 1, ipv6);
  SET_VECTOR_ELT(res, 2, port);
  SET_VECTOR_ELT(res, 3, ssl);

  UNPROTECT(5);
  return res;
}
