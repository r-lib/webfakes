
#' Run a presser app in another process
#'
#' Runs an app in a subprocess, using [callr::r_session].
#'
#' @param app `presser_app` object, the web app to run.
#' @param ... Options to pass to the [callr::r_session_options()] when
#'   setting up the subprocess.
#' @param .port Port to use. By default the OS assigns a port.
#' @param .process_timeout How long to wait for the subprocess to start, in
#'   milliseconds.
#' @return A `presser_app_process` class.
#'
#' ## Methods
#'
#' The `presser_app_process` class has the following methods:
#'
#' ```r
#' get_app()
#' get_port()
#' stop()
#' get_state()
#' url(path = "/", query = NULL)
#' ```
#'
#' * `path`: Path to return the URL for.
#' * `query`: Additional query parameters, a named list, to add to the URL.
#'
#' `get_app()` returns the app object.
#'
#' `get_port()` returns the port the web server is running on.
#'
#' `stop()` stops the web server, and also the subprocess.
#'
#' `get_state()` returns a string, the state of the web server:
#' * `"not running"` the server is not running (because it was stopped
#'   already).
#' * `"live"` means that the server is running.
#' * `"dead"` means that the subprocess has quit or crashed.
#'
#' `url()` returns the URL of the web app. You can use the `path`
#' parameter to return a specific path.
#'
#' @aliases presser_app_process
#' @export
#' @examples
#' app <- new_app()
#' app$get("/foo", function(req, res) {
#'   res$send("Hello world!")
#' })
#'
#' proc <- new_app_process(app)
#' url <- proc$url("/foo")
#' resp <- curl::curl_fetch_memory(url)
#' cat(rawToChar(resp$content))
#'
#' proc$stop()

new_app_process <- function(app, ..., .port = NULL,
                            .process_timeout = 5000) {

  app; list(...); .port; .process_timeout
  self <- new_object(
    "presser_app_process",

    new = function(app, ..., .port = NULL) {
      self$.app <- app
      opts <- callr::r_session_options(...)
      self$.process <- callr::r_session$new(opts, wait = TRUE)
      self$.process$call(
        args = list(app, .port),
        function(app, .port) {
          library(presser)
          .GlobalEnv$app <- app
          app$listen(port = .port)
        }
      )

      if (self$.process$poll_process(.process_timeout) != "ready") {
        self$.process$kill()
        stop("presser app subprocess did not start :(")
      }
      msg <- self$.process$read()
      if (msg$code == 200 && !is.null(msg$error)) {
        msg$error$message <- paste0(
          "failed to start presser app process: ",
          msg$error$message
        )
        stop(msg$error)
      }
      if (msg$code != 301) {
        stop("Unexpected message from presser app subprocess. ",
             "Report a bug please.")
      }
      self$.port <- msg$message$port

      invisible(self)
    },

    get_app = function() self$.app,

    get_port = function() self$.port,

    stop = function() {
      if (is.null(self$.process)) return(invisible(self))

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
      try_silently(self$.process$read())
      try_silently(self$.process$close())
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

    url = function(path = "/", query = NULL) {
      if (!is.null(query)) {
        query <- paste0("?", paste0(names(query), "=", query, collapse = "&"))
      }
      paste0("http://127.0.0.1:", self$.port, path, query)
    },

    .process = NULL,
    .app = NULL,
    .port = NULL
  )

  self$new(app, ..., .port = .port)
  self$new <- NULL

  self
}
