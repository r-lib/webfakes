
#' Run a presser app in another process
#'
#' Runs an app in a subprocess, using [callr::r_session].
#'
#' @param app `presser_app` object, the web app to run.
#' @param port Port to use. By default the OS assigns a port.
#' @param opts Server options. See [server_opts()] for the defaults.
#' @param process_timeout How long to wait for the subprocess to start, in
#'   milliseconds.
#' @param callr_opts Options to pass to [callr::r_session_options()]
#'   when setting up the subprocess.
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
#' local_env(envvars)
#' url(path = "/", query = NULL)
#' ```
#'
#' * `envvars`: Named list of environment variables.
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
#' `local_env()` sets the given environment variables for the duration of
#' the app process. It resets them in `$stop()`.
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

new_app_process <- function(app, port = NULL, opts = server_opts(),
                            process_timeout = 5000, callr_opts = NULL) {

  app; port; opts; process_timeout; callr_opts

  self <- new_object(
    "presser_app_process",

    new = function(app, port = NULL, opts = server_opts(), callr_opts = NULL) {
      self$.app <- app
      callr_opts <- do.call(callr::r_session_options, as.list(callr_opts))
      self$.process <- callr::r_session$new(callr_opts, wait = TRUE)
      self$.process$call(
        args = list(app, port, opts),
        function(app, port, opts) {
          library(presser)
          .GlobalEnv$app <- app
          app$listen(port = port, opts = opts)
        }
      )

      if (self$.process$poll_process(process_timeout) != "ready") {
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
      if (!is.null(self$.old_env)) set_envvar(self$.old_env)

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

    local_env = function(envvars) {
      self$.old_env <- c(self$.old_env, set_envvar(envvars))
      invisible(self)
    },

    url = function(path = "/", query = NULL) {
      if (!is.null(query)) {
        query <- paste0("?", paste0(names(query), "=", query, collapse = "&"))
      }
      paste0("http://127.0.0.1:", self$.port, path, query)
    },

    .process = NULL,
    .app = NULL,
    .port = NULL,
    .old_env = NULL
  )

  self$new(
    app,
    port = port,
    opts = opts,
    callr_opts = callr_opts
  )
  self$new <- NULL

  self
}
