#' Run a webfakes app in another process
#'
#' Runs an app in a subprocess, using [callr::r_session].
#'
#' @param app `webfakes_app` object, the web app to run.
#' @param port Port(s) to use. By default the OS assigns a port.
#'   Add an `"s"` suffix to the port to use HTTPS. Use `"0s"` to
#'   use an OS assigned port with HTTPS. See the [how-to] on how
#'   to run the web server on multiple ports.
#' @param opts Server options. See [server_opts()] for the defaults.
#' @param start Whether to start the web server immediately. If this is
#'   `FALSE`, and `auto_start` is `TRUE`, then it is started as neeed.
#' @param auto_start Whether to start the web server process automatically.
#'   If `TRUE` and the process is not running, then `$start()`,
#'   `$get_port()`, `$get_ports()` and `$url()` start the process.
#' @param process_timeout How long to wait for the subprocess to start, in
#'   milliseconds.
#' @param callr_opts Options to pass to [callr::r_session_options()]
#'   when setting up the subprocess.
#' @return A `webfakes_app_process` object.
#'
#' ## Methods
#'
#' The `webfakes_app_process` class has the following methods:
#'
#' ```r
#' get_app()
#' get_port()
#' get_ports()
#' stop()
#' get_state()
#' local_env(envvars)
#' url(path = "/", query = NULL)
#' ```
#'
#' * `envvars`: Named list of environment variables. The `{url}` substring
#'   is replaced by the URL of the app.
#' * `path`: Path to return the URL for.
#' * `query`: Additional query parameters, a named list, to add to the URL.
#'
#' `get_app()` returns the app object.
#'
#' `get_port()` returns the (first) port the web server is running on.
#'
#' `get_ports()` returns all ports the web server is running on, and
#' whether it uses SSL on those ports, in a data frame with columns
#' `ipv4`, `ipv6`, `port` and `ssl`.
#'
#' `stop()` stops the web server, and also the subprocess. If the error
#' log file is not empty, then it dumps its contents to the screen.
#'
#' `get_state()` returns a string, the state of the web server:
#' * `"not running"` the server is not running (because it was stopped
#'   already).
#' * `"live"` means that the server is running.
#' * `"dead"` means that the subprocess has quit or crashed.
#'
#' `local_env()` sets the given environment variables for the duration of
#' the app process. It resets them in `$stop()`. Webfakes replaces `{url}`
#' in the value of the environment variables with the app URL, so you can
#' set environment variables that point to the app.
#'
#' `url()` returns the URL of the web app. You can use the `path`
#' parameter to return a specific path.
#'
#' @aliases webfakes_app_process
#' @seealso [local_app_process()] for automatically cleaning up the
#'   subprocess.
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

new_app_process <- function(
  app,
  port = NULL,
  opts = server_opts(remote = TRUE),
  start = FALSE,
  auto_start = TRUE,
  process_timeout = NULL,
  callr_opts = NULL
) {
  app
  port
  opts
  start
  auto_start
  process_timeout
  callr_opts

  # WTF
  tmp <- tempfile()
  sink(tmp)
  print(app$.stack)
  sink(NULL)
  unlink(tmp)

  process_timeout <- process_timeout %||%
    as.integer(Sys.getenv(
      "R_WEBFAKES_PROCESS_TIMEOUT",
      if (Sys.getenv("R_COVR") == "true") 600000 else 5000
    ))

  self <- new_object(
    "webfakes_app_process",

    start = function() {
      self$.app <- app
      callr_opts <- do.call(callr::r_session_options, as.list(callr_opts))
      self$.process <- callr::r_session$new(callr_opts, wait = TRUE)
      self$.process$call(
        args = list(app, port, opts),
        function(app, port, opts) {
          library(webfakes)
          .GlobalEnv$app <- app
          app$listen(port = port, opts = opts)
        }
      )

      if (self$.process$poll_process(process_timeout) != "ready") {
        self$.process$kill()
        stop("webfakes app subprocess did not start :(")
      }
      msg <- self$.process$read()
      if (msg$code == 200 && !is.null(msg$error)) {
        msg$error$message <- paste0(
          "failed to start webfakes app process: ",
          msg$error$message
        )
        stop(msg$error)
      }
      if (msg$code != 301) {
        stop(
          "Unexpected message from webfakes app subprocess. ",
          "Report a bug please."
        )
      }
      self$.ports <- msg$message$ports
      self$.port <- msg$message$ports$port[1]
      self$.access_log <- msg$message$access_log
      self$.error_log <- msg$message$error_log
      self$.set_env()

      invisible(self)
    },

    get_app = function() self$.app,

    get_port = function() {
      if (self$get_state() == "not running" && auto_start) {
        self$start()
      }
      self[[".port"]]
    },

    get_ports = function() {
      if (self$get_state() == "not running" && auto_start) {
        self$start()
      }
      self[[".ports"]]
    },

    stop = function() {
      self$.reset_env()
      if (is.null(self$.process)) {
        return(invisible(self))
      }

      if (!self$.process$is_alive()) {
        status <- self$.process$get_exit_status()
        out <- err <- NULL
        try_silently(out <- self$.process$read_output())
        try_silently(err <- self$.process$read_error())
        cat0("webfakes process dead, exit code: ", status, "\n")
        if (!is.null(out)) {
          cat0("stdout:", out, "\n")
        }
        if (!is.null(err)) cat0("stderr:", err, "\n")
      }

      try_silently(self$.process$close(grace = 0))
      try_silently(self$.process$read())
      self$.print_errors()
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
      if (!is.null(self$.process)) {
        local <- unlist(envvars)
        local[] <- gsub("{url}", self$url(), local, fixed = TRUE)
        self$.old_env <- c(self$.old_env, set_envvar(local))
      } else {
        self$.local_env <- utils::modifyList(
          as.list(self$.local_env),
          as.list(envvars)
        )
      }
      invisible(self)
    },

    .set_env = function() {
      local <- unlist(self$.local_env)
      local[] <- gsub("{url}", self$url(), local, fixed = TRUE)
      self$.old_env <- set_envvar(local)
      invisible(self)
    },

    .reset_env = function() {
      if (!is.null(self$.old_env)) {
        set_envvar(self$.old_env)
        self$.old_env <- NULL
      }
    },

    url = function(
      path = "/",
      query = NULL,
      https = FALSE,
      domain = "127.0.0.1"
    ) {
      if (self$get_state() == "not running" && auto_start) {
        self$start()
      }
      if (!is.null(query)) {
        query <- paste0("?", paste0(names(query), "=", query, collapse = "&"))
      }
      port <- if (https) {
        if (!any(self$.ports$ssl)) {
          stop("Web server does support HTTPS")
        }
        self$.ports$port[self$.ports$ssl][1]
      } else {
        self$.port
      }
      proto <- if (self$.ports$ssl[match(port, self$.ports$port)]) {
        "https"
      } else {
        "http"
      }
      paste0(proto, "://", domain, ":", port, path, query)
    },

    .process = NULL,
    .app = NULL,
    .ports = NULL,
    .port = NULL,
    .old_env = NULL,
    .access_log = NA_character_,
    .error_log = NA_character_,
    .auto_start = auto_start,

    .print_errors = function() {
      if (
        !is.na(self$.error_log) &&
          file.exists(self$.error_log) &&
          file.info(self$.error_log)$size > 0
      ) {
        err <- readLines(self$.error_log, warn = FALSE)
        cat("webfakes web server errors:\n")
        cat(err, sep = "\n")
      }
    }
  )

  reg.finalizer(self, function(x) x$.reset_env())

  if (start) {
    self$start()
  }

  self
}
