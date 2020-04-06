
#' @export

new_app_process <- function(app, ...) {

  list(...)
  self <- new_object(
    "presser_app_process",

    new = function(app, ...) {
      self$.app <- app
      opts <- callr::r_session_options(...)
      self$.process <- callr::r_session$new(opts, wait = TRUE)
      self$.process$call(
        args = list(app),
        function(app) {
          library(presser)
          .GlobalEnv$app <- app
          app$listen(port = NULL)
        }
      )

      if (self$.process$poll_process(5000) != "ready") {
        stop("app subprocess did not start :(")
      }
      msg <- self$.process$read()
      self$.port <- msg$message$port

      invisible(self)
    },

    get_url = function(path = "/", query = NULL) {
      if (!is.null(query)) {
        query <- paste0("?", paste0(names(query), "=", query, collapse = "&"))
      }
      paste0("http://127.0.0.1:", self$.port, path, query)
    },

    get_port = function() self$.port,

    stop = function() {
      # The details are important here, for the sake of covr,
      # so that we can test the presser package itself.
      # 1. The subprocess serving the app is in Sys.sleep(), which we
      #    need to interrupt first.
      # 2. Then we need to read out the result of that $call()
      #    (i.e. the interruption), because otherwise the subprocess is
      #    stuck at a blocking write() system call, and cannot be
      #    interrupted in the $close() call, and will be killed, and
      #    then it cannot write out the coverage results.
      # 3. Once we $read(), we can call $close() because that will
      #    close the standard input of the subprocess, which is reading
      #    the standard input, so it will quit.

      self$.process$interrupt()
      self$.process$poll_process(1000)
      self$.process$read()
      self$.process$close()
      self$.process <- NULL
      invisible(self)
    },

    get_state = function() {
      if (is.null(self$.process)) {
        "not running"
      } else if (self$.process$is_alive()) {
        "live"
      } else {
        "dead"
      }
    },

    .process = NULL,
    .app = NULL,
    .port = NULL
  )

  self$new(app, ...)
  self$new <- NULL

  self
}
