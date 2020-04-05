
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <pthread.h>
#include <time.h>

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
  /* Once we require some features we need to check the return value. */
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
  struct mg_server_port ports[4];
  int num_ports;
};

static int begin_request(struct mg_connection *conn) {

  struct mg_context *ctx = mg_get_context(conn);
  struct presser_server *srv = mg_get_user_data(ctx);

  /* TODO: log errors into the web error log. */
  /* TODO: what happens to the locks on error, we should release them? */

  if (pthread_mutex_lock(&srv->process_lock)) return 1;
  srv->conn = conn;
  if (pthread_cond_signal(&srv->process_cond)) return 1;
  if (pthread_mutex_unlock(&srv->process_lock)) return 1;

  /* Need to wait for the response... */
  if (pthread_mutex_lock(&srv->finish_lock)) return 1;
  while (srv->response_ok == 0) {
    if (pthread_cond_wait(&srv->finish_cond, &srv->finish_lock)) return 1;
  }

  srv->response_ok = 0;

  /* we can return now, whatever the return value here */
  pthread_mutex_unlock(&srv->finish_lock);
  return 1;
}

static void presser_server_finalizer(SEXP rsrv) {
  /* TODO: what if a thread is waiting on one of these right now? */
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  int ret = 0;
  R_ClearExternalPtr(rsrv);
  mg_stop(srv->ctx);
  ret += pthread_mutex_destroy(&srv->process_lock);
  ret += pthread_cond_destroy(&srv->process_cond);
  ret += pthread_mutex_destroy(&srv->finish_lock);
  ret += pthread_cond_destroy(&srv->finish_cond);
  if (ret) warning("Error while cleaning up presser web server");
}

#define CHK(expr) if ((ret = expr))                                     \
    R_THROW_SYSTEM_ERROR_CODE(ret, "Cannot start presser web server")

SEXP server_start(SEXP options) {

  SEXP rsrv = R_NilValue;
  struct presser_server *srv = malloc(sizeof(struct presser_server));
  if (!srv) R_THROW_SYSTEM_ERROR("Cannot start presser server");
  int ret = 0;

  memset(srv, 0, sizeof(struct presser_server));
  PROTECT(rsrv = R_MakeExternalPtr(srv, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(rsrv, presser_server_finalizer);

  CHK(pthread_cond_init(&srv->process_cond, NULL));
  CHK(pthread_mutex_init(&srv->process_lock, NULL));
  CHK(pthread_cond_init(&srv->finish_cond, NULL));
  CHK(pthread_mutex_init(&srv->finish_lock, NULL));

  /* TODO */
  const char *coptions[] = {
    "listening_ports",    "3000",
    "request_timeout_ms", "100000",
    "num_threads",        "1",
    0
  };

  struct mg_callbacks callbacks;

  memset(&callbacks, 0, sizeof(callbacks));
  callbacks.begin_request = begin_request;
  srv->ctx = mg_start(&callbacks, srv, coptions);

  if (srv->ctx == NULL) R_THROW_ERROR("Cannot start presser web server");

  memset(srv->ports, 0, sizeof(srv->ports));
  srv->num_ports = mg_get_server_ports(
    srv->ctx,
    sizeof(srv->ports) / sizeof(struct mg_server_port),
    srv->ports
  );
  if (srv->num_ports < 0) R_THROW_ERROR("Cannot get presser web server ports");

  CHK(pthread_mutex_lock(&srv->process_lock));

  UNPROTECT(1);
  return rsrv;
}

#undef CHK
#define CHK(expr) if ((ret = expr))                                     \
    R_THROW_SYSTEM_ERROR_CODE(ret, "Cannot process presser web server requests")

SEXP server_process(SEXP rsrv, SEXP handler, SEXP env) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv == NULL) error("presser server has stopped already");

  int ret;
  struct timespec timeout;

  while (1) {
    while (srv->conn == NULL) {
      clock_gettime(CLOCK_REALTIME, &timeout);
      ret = ETIMEDOUT;
      while (ret == ETIMEDOUT) {
        R_CheckUserInterrupt();
        timeout.tv_sec += 1;
        ret = pthread_cond_timedwait(
          &srv->process_cond, &srv->process_lock, &timeout);
      }
    }

    /* Actual request processing, and setting the global response
       object here. */

    const char *body = "1234567890";
    if (mg_send_http_ok(srv->conn, "text/plain", 10) < 0) {
      R_THROW_ERROR("Failed to send HTTP response");
    }
    if (mg_write(srv->conn, body, 10) < 0) {
      R_THROW_ERROR("Failed to write HTTP response");
    }

    srv->conn = 0;
    CHK(pthread_mutex_unlock(&srv->process_lock));

    CHK(pthread_mutex_lock(&srv->finish_lock));
    srv->response_ok = 1;
    CHK(pthread_cond_signal(&srv->finish_cond));
    CHK(pthread_mutex_unlock(&srv->finish_lock));
  }

  return R_NilValue;
}
