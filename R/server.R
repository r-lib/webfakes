
server_start <- function(opts = server_opts()) {
  ports <- paste0(opts$interfaces, ":", opts$port %||% "0", collapse = ",")

  if (!is.na(opts$access_log_file)) {
    mkdirp(dirname(opts$access_log_file))
    file.create(opts$access_log_file)
  }
  if (!is.na(opts$error_log_file)) {
    mkdirp(dirname(opts$error_log_file))
    file.create(opts$error_log_file)
  }

  options <- c(
    "listening_ports"          = ports,
    "num_threads"              = opts$num_threads,
    "enable_keep_alive"        = c("no", "yes")[[opts$enable_keep_alive + 1]],
    "access_log_file"          = opts$access_log_file %|NA|% "",
    "error_log_file"           = opts$error_log_file %|NA|% "",

    # These are not configurable currently
    "request_timeout_ms"       = "100000",
    "enable_auth_domain_check" = "no"
  )

  srv <- .Call(c_server_start, options)
  attr(srv, "options") <- opts
  srv
}

#' Presser web server options
#'
#' @param remote Meta-option. If set to `TRUE`, presser uses slightly
#'   different defaults, that are more appropriate for a background
#'   server process.
#' @param port Port to start the web server on. Defaults to a randomly
#'   chosen port.
#' @param num_threads Number of request handler threads to use. Typically
#'   you don't need more than one thread, unless you run test cases in
#'   parallel or you make concurrent HTTP requests.
#' @param interfaces The network interfaces to listen on. Being a test
#'   web server, it defaults to the localhost. Only bind to a public
#'   interface if you know what you are doing. presser was not designed
#'   to serve public web pages.
#' @param enable_keep_alive Whether the server keeps connections alive.
#' @param access_log_file `TRUE`, `FALSE`, or a path. See 'Logging'
#'   below.
#' @param error_log_file `TRUE`, `FALSE`, or a path. See 'Logging'
#'   below.
#'
#' @section Logging:
#'
#' * For `access_log_file`, `TRUE` means `<log-dir>/access.log`.
#' * For `error_log_file`, `TRUE` means `<log-dir>/error.log`.
#'
#' `<log-dir>` is set to the contents of the `PRESSER_LOG_DIR`
#' environment variable, if it is set. Otherwise it is set to
#' `<tmpdir>/presser` for local apps and `<tmpdir>/<pid>/presser` for
#' remote apps (started with `new_app_procss()`).
#'
#' `<tmpdir>` is the session temporary directory of the _main process_.
#'
#' `<pid>` is the process id of the subprocess.
#'
#' @export

server_opts <- function(remote = FALSE, port = NULL, num_threads = 1,
                        interfaces = "127.0.0.1",
                        enable_keep_alive = FALSE,
                        access_log_file = remote,
                        error_log_file = TRUE) {

  log_dir <- Sys.getenv("PRESSER_LOG_DIR", file.path(tempdir(), "presser"))
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

  as.list(environment())
}

server_get_ports <- function(srv) {
  as.data.frame(.Call(c_server_get_ports, srv))
}

server_process <- function(srv, handler) {
  done <- FALSE
  while (!done) {
    tryCatch(
      call_with_cleanup(c_server_process, srv, handler, environment()),
      error = function(err) NULL,
      interrupt = function(err) done <<- TRUE
    )
  }

  invisible(srv)
}

server_stop <- function(srv) {
  invisible(.Call(c_server_stop, srv))
}
