
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <pthread.h>

#include "civetweb.h"
#include "errors.h"

SEXP server_start(SEXP options);
SEXP server_process(SEXP rsrv, SEXP handler, SEXP env);

static const R_CallMethodDef callMethods[]  = {
  { "server_start",   (DL_FUNC) &server_start,   1 },
  { "server_process", (DL_FUNC) &server_process, 3 },
  { NULL, NULL, 0 }
};

void R_init_presser(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  mg_init_library(0);
}

struct presser_server {
  struct mg_context *ctx;
  pthread_cond_t process_cond;  /* can process? */
  pthread_mutex_t process_lock;
  pthread_cond_t finish_cond;   /* can finish callback? */
  pthread_mutex_t finish_lock;
  struct mg_connection *conn ;
  int response_ok;
  int port;
};

static int log_message(const struct mg_connection *conn, const char *message) {
  REprintf(message);
  return 1;
}

static int begin_request(struct mg_connection *conn) {

  struct mg_context *ctx = mg_get_context(conn);
  struct presser_server *srv = mg_get_user_data(ctx);
  int ret;

  fprintf(stderr, "worker: thread\n");
  ret = pthread_mutex_lock(&srv->process_lock);
  if (ret) {
    fprintf(stderr, "worker: cannot lock process: %d\n", ret);
    return 1;
  }
  srv->conn = conn;
  pthread_cond_signal(&srv->process_cond);
  pthread_mutex_unlock(&srv->process_lock);

  /* Need to wait for the response... */
  fprintf(stderr, "worker: waiting for response\n");
  pthread_mutex_lock(&srv->finish_lock);
  while (srv->response_ok == 0) {
    pthread_cond_wait(&srv->finish_cond, &srv->finish_lock);
  }

  srv->response_ok = 0;
  fprintf(stderr, "worker: response sent\n");

  /* we can return now */
  pthread_mutex_unlock(&srv->finish_lock);
  return 1;
}

static int begin_request2(struct mg_connection *conn) {
  const struct mg_request_info *req = mg_get_request_info(conn);
  if (req == NULL) {
    warning("Internal presser web server error when processing request");
  }

  REprintf("request!\n");
  const char *rreq_names[] = {
    "method",                   /* 0 */
    "request_uri",              /* 1 */
    "local_uri",                /* 2 */
    "http_version",             /* 3 */
    "query_string",             /* 4 */
    "remote_addr",              /* 5 */
    "content_length",           /* 6 */
    "remote_port",              /* 7 */
    "headers",                  /* 8 */
    ""
  };
  SEXP rreq = PROTECT(Rf_mkNamed(VECSXP, rreq_names));
  //  SET_VECTOR_ELT(rreq, 0, mkString(req->request_method));

  SEXP handler = PROTECT(install("server_handler"));
  SEXP presser = PROTECT(mkString("presser"));
  // SEXP ns = PROTECT(R_FindNamespace(presser));
  // SEXP call = PROTECT(lang2(handler, rreq));
  // SEXP ret = PROTECT(Rf_eval(call, ns));

  /* TODO: handle ret */
  // (void) ret;

  REprintf("Sending ok\n");
  const char *body = "1234567890";
  mg_send_http_ok(conn, "text/plain", 10);
  REprintf("Sending data\n");
  mg_write(conn, body, 10);

  REprintf("request done\n");

  UNPROTECT(4);
  return 200;
}

static void end_request(const struct mg_connection *conn,
                        int reply_status_code) {
  /* TODO */
}

static void presser_server_finalizer(SEXP rsrv) {
  /* TODO: what if a thread is waiting on one of these right now? */
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  R_ClearExternalPtr(rsrv);
  mg_stop(srv->ctx);
  pthread_mutex_destroy(&srv->process_lock);
  pthread_cond_destroy(&srv->process_cond);
  pthread_mutex_destroy(&srv->finish_lock);
  pthread_cond_destroy(&srv->finish_cond);
}

SEXP server_start(SEXP options) {

  SEXP rsrv = R_NilValue;
  struct presser_server *srv = malloc(sizeof(struct presser_server));
  if (!srv) error("Cannot start presser server, out of memory");

  srv->ctx = 0;
  srv->conn = 0;
  srv->response_ok = 0;
  srv->port = 0;
  pthread_cond_init(&srv->process_cond, NULL);
  pthread_mutex_init(&srv->process_lock, NULL);
  pthread_cond_init(&srv->finish_cond, NULL);
  pthread_mutex_init(&srv->finish_lock, NULL);

  PROTECT(rsrv = R_MakeExternalPtr(srv, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(rsrv, presser_server_finalizer);

  /* TODO */
  const char *coptions[] = {
    "listening_ports",    "3000",
    "request_timeout_ms", "100000",
    "num_threads",        "1",
    0
  };

  struct mg_callbacks callbacks;
  struct mg_server_port ports[32];

  memset(&callbacks, 0, sizeof(callbacks));
  callbacks.log_message = log_message;
  callbacks.begin_request = begin_request;
  callbacks.end_request = end_request;
  srv->ctx = mg_start(&callbacks, srv, coptions);

  if (srv->ctx == NULL) error("Cannot start presser web server");
  REprintf("started\n");

  memset(ports, 0, sizeof(ports));
  srv->port = mg_get_server_ports(srv->ctx, 32, ports);

  pthread_mutex_lock(&srv->process_lock);

  UNPROTECT(1);
  return rsrv;
}

SEXP server_process(SEXP rsrv, SEXP handler, SEXP env) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) error("presser server has stopped already");

  int ret;

  while (1) {
    fprintf(stderr, "R: waiting for request\n");
    while (srv->conn == NULL) {
      ret = pthread_cond_wait(&srv->process_cond, &srv->process_lock);
      if (ret) {
        R_THROW_SYSTEM_ERROR_CODE
          (ret, "presser web server error waiting for requests");
      }
    }
    fprintf(stderr, "R: processing request\n");
    /* Actual request processing, and setting the global response
       object here. */

    const char *body = "1234567890";
    mg_send_http_ok(srv->conn, "text/plain", 10);
    mg_write(srv->conn, body, 10);
    fprintf(stderr, "R: response sent\n");

    srv->conn = 0;
    pthread_mutex_unlock(&srv->process_lock);

    pthread_mutex_lock(&srv->finish_lock);
    srv->response_ok = 1;
    pthread_cond_signal(&srv->finish_cond);
    pthread_mutex_unlock(&srv->finish_lock);
  }

  return R_NilValue;
}
