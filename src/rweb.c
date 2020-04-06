
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <pthread.h>
#include <time.h>

#include "civetweb.h"
#include "errors.h"

SEXP server_start(SEXP options);
SEXP server_process(SEXP rsrv, SEXP handler, SEXP env);
SEXP server_stop(SEXP rsrv);

static const R_CallMethodDef callMethods[]  = {
  { "server_start",   (DL_FUNC) &server_start,   1 },
  { "server_process", (DL_FUNC) &server_process, 3 },
  { "server_stop",    (DL_FUNC) &server_stop,    1 },
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
  if (srv == NULL) return;
  int ret = 0;
  R_ClearExternalPtr(rsrv);
  srv->response_ok = 1;
  ret += pthread_mutex_unlock(&srv->process_lock);
  ret += pthread_mutex_unlock(&srv->finish_lock);
  ret += pthread_cond_signal(&srv->finish_cond);
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
  if (srv == NULL) R_THROW_ERROR("presser server has stopped already");

  int ret, i;

  while (1) {
    while (srv->conn == NULL) {
      R_CheckUserInterrupt();
      /* TODO: wake up handler callback to avoid a locked server */
      ret = pthread_cond_wait(&srv->process_cond, &srv->process_lock);
    }

    /* Actual request processing */

    const struct mg_request_info *req = mg_get_request_info(srv->conn);
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
    SET_VECTOR_ELT(rreq, 0, mkString(req->request_method));
    SET_VECTOR_ELT(rreq, 1, mkString(req->request_uri));
    SET_VECTOR_ELT(rreq, 2, mkString(req->local_uri));
    SET_VECTOR_ELT(rreq, 3, mkString(req->http_version));
    SET_VECTOR_ELT(rreq, 4, req->query_string ? mkString(req->query_string) : mkString(""));
    SET_VECTOR_ELT(rreq, 5, mkString(req->remote_addr));
    SET_VECTOR_ELT(rreq, 6, ScalarReal(req->content_length));
    SET_VECTOR_ELT(rreq, 7, ScalarInteger(req->remote_port));

    SEXP hdr = PROTECT(allocVector(VECSXP, req->num_headers));
    SEXP nms = PROTECT(allocVector(STRSXP, req->num_headers));
    for (i = 0; i < req->num_headers; i++) {
      SET_VECTOR_ELT(hdr, i, mkString(req->http_headers[i].value));
      SET_STRING_ELT(nms, i, mkChar(req->http_headers[i].name));
    }
    Rf_setAttrib(hdr, R_NamesSymbol, nms);
    SET_VECTOR_ELT(rreq, 8, hdr);

    SEXP call = PROTECT(lang2(handler, rreq));
    SEXP res = PROTECT(eval(call, env));
    int ret;

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
        req->http_version, len
      );
      if (ret < 0) R_THROW_ERROR("Could not send HTTP error response");
      if (mg_write(srv->conn, s, len) < 0) {
        R_THROW_ERROR("Failed to write HTTP response body");
      }

    } else if (TYPEOF(res) == VECSXP && LENGTH(res) == 4) {
      SEXP cnt = VECTOR_ELT(res, 0);
      SEXP sct = VECTOR_ELT(res, 1);
      SEXP hdr = VECTOR_ELT(res, 2);
      int code = INTEGER(VECTOR_ELT(res, 3))[0];
      const char *ct_raw = "application/octet-stream";
      const char *ct_str = "text/plain";
      const char *ct;
      int clen = 0;
      if (!isNull(sct)) {
        ct = CHAR(STRING_ELT(sct, 0));
      } else if (TYPEOF(cnt) == RAWSXP) {
        ct = ct_raw;
      } else if (TYPEOF(cnt) == STRSXP) {
        ct = ct_str;
      } else {
        R_THROW_ERROR("Invalid content type for HTTP response");
      }

      if (TYPEOF(cnt) == RAWSXP) {
        clen = LENGTH(cnt);
      } else {
        clen = strlen(CHAR(STRING_ELT(cnt, 0)));
      }

      ret = mg_printf(
        srv->conn,
        "HTTP/%s %d %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %d\r\n",
        req->http_version,
        code, mg_get_response_code_text(srv->conn, code),
        ct, clen
      );
      if (ret < 0) R_THROW_ERROR("Could not send HTTP response");

      for (i = 0; i < LENGTH(hdr); i++) {
        const char *hs = CHAR(STRING_ELT(hdr, i));
        ret = mg_write(srv->conn, hs, strlen(hs));
        ret |= mg_write(srv->conn, "\r\n", 2);
        if (ret < 0) R_THROW_ERROR("Could not send HTTP response");
      }

      if (mg_write(srv->conn, "\r\n", 2) < 0) {
        R_THROW_ERROR("Could not send HTTP response");
      }
      if (TYPEOF(cnt) == RAWSXP) {
        ret = mg_write(srv->conn, RAW(cnt), clen);
      } else {
        /* TODO: send a file */
        ret = mg_write(srv->conn, CHAR(STRING_ELT(cnt, 0)), clen);
      }
      if (ret < 0) R_THROW_ERROR("Could not send HTTP response");

    } else if (isNull(res)) {
      /* Do nothing. Response is sent or empty response */

    } else {
      R_THROW_ERROR("Invalid presser response");
    }

    UNPROTECT(6);

    /* OK, we are done */

    srv->conn = 0;
    CHK(pthread_mutex_unlock(&srv->process_lock));

    CHK(pthread_mutex_lock(&srv->finish_lock));
    srv->response_ok = 1;
    CHK(pthread_cond_signal(&srv->finish_cond));
    CHK(pthread_mutex_unlock(&srv->finish_lock));
  }

  return R_NilValue;
}

SEXP server_stop(SEXP rsrv) {
  struct presser_server *srv = R_ExternalPtrAddr(rsrv);
  if (srv != NULL) presser_server_finalizer(rsrv);
  return R_NilValue;
}
