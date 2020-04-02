
#' @export

new_app_process <- function(app) {

  self <- new_object(
    "pressr_app_process",

    new = function(app) {
      self$.app <- app
      self$.process <- callr::r_session$new(wait = TRUE)
      self$.port <- self$.process$run(
        args = list(app),
        function(app) {
          library(pressr)
          .GlobalEnv$app <- app
          app$listen(port = NULL, block = FALSE)
          app$get_port()
        }
      )

      self$.process$call(
        function() while(TRUE) Sys.sleep(1000)
      )

      invisible(self)
    },

    get_url = function(...) {
      paste(
        c(paste0("http://127.0.0.1:", self$.port), ...),
        collapse = "/"
      )
    },

    get_port = function() self$.port,

    stop = function() {
      # The details are important here, for the sake of covr,
      # so that we can test the pressr package itself.
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

  self$new(app)
  self$new <- NULL

  self
}
