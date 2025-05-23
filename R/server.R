parse_ports <- function(opts = server_opts()) {
  paste0(
    if (!is.null(opts$interfaces)) paste0(opts$interfaces, ":"),
    opts$port %||% "0",
    collapse = ","
  )
}

server_start <- function(opts = server_opts()) {
  ports <- parse_ports(opts)

  if (!is.na(opts$access_log_file)) {
    mkdirp(dirname(opts$access_log_file))
    file.create(opts$access_log_file)
  }
  if (!is.na(opts$error_log_file)) {
    mkdirp(dirname(opts$error_log_file))
    file.create(opts$error_log_file)
  }

  throttle <- paste0(
    "*=",
    if (opts$throttle == Inf) "0" else opts$throttle
  )

  options <- c(
    "listening_ports" = ports,
    "num_threads" = opts$num_threads,
    "enable_keep_alive" = c("no", "yes")[[opts$enable_keep_alive + 1]],
    "access_log_file" = opts$access_log_file %|NA|% "",
    "error_log_file" = opts$error_log_file %|NA|% "",
    "tcp_nodelay" = c("0", "1")[[opts$tcp_nodelay + 1]],
    "throttle" = throttle,

    # These are not configurable currently
    "request_timeout_ms" = "100000",
    "enable_auth_domain_check" = "no",
    "decode_url" = if (opts$decode_url) "yes" else "no",
    "ssl_certificate" = if (!is.null(opts$ssl_certificate)) opts$ssl_certificate
  )

  srv <- call_with_cleanup(c_server_start, options)
  attr(srv, "options") <- opts
  srv
}

#' Webfakes web server options
#'
#' @param remote Meta-option. If set to `TRUE`, webfakes uses slightly
#'   different defaults, that are more appropriate for a background
#'   server process.
#' @param port Port to start the web server on. Defaults to a randomly
#'   chosen port.
#' @param num_threads Number of request handler threads to use. Typically
#'   you don't need more than one thread, unless you run test cases in
#'   parallel or you make concurrent HTTP requests.
#' @param interfaces The network interfaces to listen on. Being a test
#'   web server, it defaults to the localhost. Only bind to a public
#'   interface if you know what you are doing. webfakes was not designed
#'   to serve public web pages.
#' @param enable_keep_alive Whether the server keeps connections alive.
#' @param access_log_file `TRUE`, `FALSE`, or a path. See 'Logging'
#'   below.
#' @param error_log_file `TRUE`, `FALSE`, or a path. See 'Logging'
#'   below.
#' @param tcp_nodelay if `TRUE` then packages will be sent as soon as
#'   possible, instead of waiting for a full buffer or timeout to occur.
#' @param throttle Limit download speed for clients. If not `Inf`,
#'   then it is the maximum number of bytes per second, that is sent to
#'   as connection.
#' @param decode_url Whether the server should automatically decode
#'   URL-encodded URLs. If `TRUE` (the default), `/foo%2fbar` will be
#'   converted to `/foo/bar` automatically. If `FALSE`, URLs as not
#'   URL-decoded.
#' @param ssl_certificate Path to the SSL certificate of the server,
#'   needed if you want to server HTTPS requests.
#' @return List of options that can be passed to `webfakes_app$listen()`
#'   (see [new_app()]), and [new_app_process()].
#'
#' @section Logging:
#'
#' * For `access_log_file`, `TRUE` means `<log-dir>/access.log`.
#' * For `error_log_file`, `TRUE` means `<log-dir>/error.log`.
#'
#' `<log-dir>` is set to the contents of the `WEBFAKES_LOG_DIR`
#' environment variable, if it is set. Otherwise it is set to
#' `<tmpdir>/webfakes` for local apps and `<tmpdir>/<pid>/webfakes` for
#' remote apps (started with `new_app_procss()`).
#'
#' `<tmpdir>` is the session temporary directory of the _main process_.
#'
#' `<pid>` is the process id of the subprocess.
#'
#' @export
#' @examples
#' # See the defaults
#' server_opts()

server_opts <- function(
  remote = FALSE,
  port = NULL,
  num_threads = 1,
  interfaces = "127.0.0.1",
  enable_keep_alive = FALSE,
  access_log_file = remote,
  error_log_file = TRUE,
  tcp_nodelay = FALSE,
  throttle = Inf,
  decode_url = TRUE,
  ssl_certificate = NULL
) {
  log_dir <- Sys.getenv("WEBFAKES_LOG_DIR", file.path(tempdir(), "webfakes"))
  if (isTRUE(access_log_file)) {
    if (remote) {
      access_log_file <- file.path(log_dir, "%p", "access.log")
    } else {
      access_log_file <- file.path(log_dir, "access.log")
    }
  } else if (isFALSE(access_log_file)) {
    access_log_file <- NA_character_
  }
  if (isTRUE(error_log_file)) {
    if (remote) {
      error_log_file <- file.path(log_dir, "%p", "error.log")
    } else {
      error_log_file <- file.path(log_dir, "error.log")
    }
  } else if (isFALSE(error_log_file)) {
    error_log_file <- NA_character_
  }
  rm(log_dir)

  ssl_certificate <- ssl_certificate %||%
    system.file("cert/localhost/server.pem", package = "webfakes")

  as.list(environment())
}

server_get_ports <- function(srv) {
  as.data.frame(call_with_cleanup(c_server_get_ports, srv))
}

server_stop <- function(srv) {
  invisible(call_with_cleanup(c_server_stop, srv))
}

server_poll <- function(srv, cleanup = TRUE) {
  while (TRUE) {
    tryCatch(
      return(call_with_cleanup(c_server_poll, srv, cleanup)),
      error = function(err) {
        cat(as.character(err), file = stderr())
        stop(new_webfakes_error())
      }
    )
  }
}

response_send <- function(req) {
  tryCatch(
    call_with_cleanup(c_response_send, req),
    error = function(err) {
      cat(as.character(err), file = stderr())
      stop(new_webfakes_error())
    }
  )
  invisible(NULL)
}

response_send_chunk <- function(req, data) {
  tryCatch(
    call_with_cleanup(c_response_send_chunk, req, data),
    error = function(err) {
      cat(as.character(err), file = stderr())
      stop(new_webfakes_error())
    }
  )
  invisible(NULL)
}

response_send_error <- function(req, msg, status) {
  tryCatch(
    call_with_cleanup(c_response_send_error, req, msg, status),
    error = function(err) {
      cat(as.character(err), file = stderr())
      stop(new_webfakes_error())
    }
  )
  invisible(NULL)
}

response_delay <- function(req, secs) {
  tryCatch(
    call_with_cleanup(c_response_delay, req, secs),
    error = function(err) {
      cat(as.character(err), file = stderr())
      stop(new_webfakes_error())
    }
  )
  invisible(NULL)
}

response_write <- function(req, data) {
  tryCatch(
    call_with_cleanup(c_response_write, req, data),
    error = function(err) {
      cat(as.character(err), file = stderr())
      stop(new_webfakes_error())
    }
  )
  invisible(NULL)
}

new_webfakes_error <- function(...) {
  structure(
    list(message = "error in webfakes connection", ...),
    class = c("webfakes_error", "error", "condition")
  )
}
