
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <pthread.h>

#include "civetweb.h"

SEXP server_start(SEXP options);

static const R_CallMethodDef callMethods[]  = {
  { "server_start", (DL_FUNC) &server_start,  1 },
  { NULL, NULL, 0 }
};

void R_init_presser(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  mg_init_library(0);
}

static pthread_cond_t presser_cond;
static pthread_mutex_t presser_mutex;
static pthread_cond_t presser_cond2;
static pthread_mutex_t presser_mutex2;
static struct mg_connection *presser_conn = 0;
static int presser_response = 0;

static int log_message(const struct mg_connection *conn, const char *message) {
  REprintf(message);
  return 1;
}

static int begin_request(struct mg_connection *conn) {

  fprintf(stderr, "worker: thread\n");
  pthread_mutex_lock(&presser_mutex);
  presser_conn = conn;
  pthread_cond_signal(&presser_cond);
  pthread_mutex_unlock(&presser_mutex);

  /* Need to wait for the response... */
  fprintf(stderr, "worker: waiting for response\n");
  pthread_mutex_lock(&presser_mutex2);
  while (presser_response == 0) {
    pthread_cond_wait(&presser_cond2, &presser_mutex2);
  }

  presser_response = 0;
  fprintf(stderr, "worker: response sent\n");

  /* we can return now */
  pthread_mutex_unlock(&presser_mutex2);
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

SEXP server_start(SEXP options) {

  pthread_cond_init(&presser_cond, NULL);
  pthread_mutex_init(&presser_mutex, NULL);
  pthread_cond_init(&presser_cond2, NULL);
  pthread_mutex_init(&presser_mutex2, NULL);

  /* TODO */
  const char *coptions[] = {
    "listening_ports",    "3000",
    "request_timeout_ms", "100000",
    "num_threads",        "1",
    0
  };

  struct mg_callbacks callbacks;
  struct mg_context *ctx;
  struct mg_server_port ports[32];
  int port_cnt;

  memset(&callbacks, 0, sizeof(callbacks));
  callbacks.log_message = log_message;
  callbacks.begin_request = begin_request;
  callbacks.end_request = end_request;
  ctx = mg_start(&callbacks, 0, coptions);

  REprintf("started\n");

  if (ctx == NULL) error("Cannot start presser web server");

  memset(ports, 0, sizeof(ports));
  port_cnt = mg_get_server_ports(ctx, 32, ports);

  pthread_mutex_lock(&presser_mutex);
  while (1) {
    fprintf(stderr, "R: waiting for request\n");
    while (presser_conn == 0) {
      pthread_cond_wait(&presser_cond, &presser_mutex);
    }
    fprintf(stderr, "R: processing request\n");
    /* Actual request processing, and setting the global response
       object here. */

    const char *body = "1234567890";
    mg_send_http_ok(presser_conn, "text/plain", 10);
    mg_write(presser_conn, body, 10);
    fprintf(stderr, "R: response sent\n");

    presser_conn = 0;
    pthread_mutex_unlock(&presser_mutex);

    pthread_mutex_lock(&presser_mutex2);
    presser_response = 1;
    pthread_cond_signal(&presser_cond2);
    pthread_mutex_unlock(&presser_mutex2);
  }

  mg_stop(ctx);
  pthread_mutex_destroy(&presser_mutex);
  pthread_cond_destroy(&presser_cond);
  pthread_mutex_destroy(&presser_mutex2);
  pthread_cond_destroy(&presser_cond2);

  return R_NilValue;
}
