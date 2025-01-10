
#' Middleware that calls a CGI script
#'
#' You can use it as an unconditional middleware in `app$use()`,
#' as a handler on `app$get()`, `app$post()`, etc., or you can call it
#' from a handler. See examples below.
#'
#' @param command External command to run.
#' @param args Arguments to pass to the external command.
#' @param timeout Timeout for the external command. If the command does
#'   not terminate in time, the web server kills it and returns an 500
#'   response.
#' @return A function with signature
#' ```
#' function(req, res, env = character())
#' ```
#'
#' See [RFC 3875](https://www.ietf.org/rfc/rfc3875) for details on the CGI
#' protocol.
#'
#' The request body (if any) is passed to the external command as standard
#' intput. `mw_cgi()` sets `CONTENT_LENGTH`, `CONTENT_TYPE`,
#' `GATEWAY_INTERFACE`, `PATH_INFO`, `QUERY_STRING`, `REMOTE_ADDR`,
#' `REMOTE_HOST`, `REMOTE_USER`, `REQUEST_METHOD`, `SERVER_NAME`,
#' `SERVER_PORT`, `SERVER_PROTOCOL`, `SERVER_SOFTEWARE`.
#'
#' It does not currently set the `AUTH_TYPE`, `PATH_TRANSLATED`,
#' `REMOTE_IDENT`, `SCRIPT_NAME` environment variables.
#'
#' The standard output of the external command is used to set the
#' response status code, the response headers and the response body.
#' Example output from git's CGI:
#' ```
#' Status: 200 OK
#' Expires: Fri, 01 Jan 1980 00:00:00 GMT
#' Pragma: no-cache
#' Cache-Control: no-cache, max-age=0, must-revalidate
#' Content-Type: application/x-git-upload-pack-advertisement
#'
#' 000eversion 2
#' 0015agent=git/2.42.0
#' 0013ls-refs=unborn
#' 0020fetch=shallow wait-for-done
#' 0012server-option
#' 0017object-format=sha1
#' 0010object-info
#' 0000
#' ```
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_cgi("echo", "Status: 200\n\nHello"))
#' app
#'
#' app2 <- new_app()
#' app2$get("/greet", mw_cgi("echo", "Status: 200\n\nHello"))
#' app2
#'
#' # Using `mw_cgi()` in a handler, you can pass extra environment variables
#' app3 <- new_app()
#' cgi <- mw_cgi("echo", "Status: 200\n\nHello")
#' app2$get("/greet", function(req, res) {
#'   cgi(req, res, env = c("EXTRA_VAR" = "EXTRA_VALUE"))
#' })
#' app3

mw_cgi <- function(command, args = character(),
                   timeout = as.difftime(Inf, units = "secs")) {
  command
  args
  timeout <- if (timeout == Inf) {
    -1
  } else {
    as.double(timeout, units = "secs") * 1000
  }

  function(req, res, env = character()) {
    all_env <- c("current", cgi_env(req), env)

    inp <- tempfile()
    out <- tempfile()
    err <- tempfile()
    on.exit(unlink(c(inp, out, err)), add = TRUE)
    writeBin(req$.body %||% raw(), inp)

    px <- processx::process$new(
      command,
      args,
      env = all_env,
      stdin = inp,
      stdout = out,
      stderr = err
    )
    px$wait(timeout)
    output <- parse_cgi_output(px, out, err)

    res$set_status(output$status)
    for (idx in seq_along(output$headers)) {
      res$set_header(names(output$headers)[idx], output$headers[[idx]])
    }
    res$send(output$body)
  }
}

parse_cgi_output <- function(px, out, err) {
  if (px$is_alive() || px$get_exit_status() != 0) {
    px$kill()
    err <- tryCatch(read_char(err), error = function(e) "???")
    return(list(
      status = 500L,
      headers = c("content-type" = "text/plain"),
      body = paste0("Internal git error: ", err)
    ))
  }

  out <- read_bin(out)
  err <- read_char(err)

  cgi_res <- split_cgi_output(out)
  headers <- cgi_res$headers
  names(headers) <- tolower(names(headers))

  status <- parse_status(headers[["status"]] %||% "200")
  headers <- headers[names(headers) != "status"]

  list(status = status, headers = headers, body = cgi_res$body)
}

cgi_env <- function(req) {
  url <- parse_url(req$url)
  c(
    CONTENT_LENGTH = length(req$.body),
    CONTENT_TYPE = if (!is.null(req$get_header)) req$get_header("content-type") %||% "",
    GATEWAY_INTERFACE = "CGI/1.1",
    PATH_INFO = req$path,
    QUERY_STRING = req$query_string,
    REMOTE_ADDR = req$remote_addr,
    REMOTE_HOST = req$remote_addr,
    REMOTE_USER = "anonymous",
    REQUEST_METHOD = toupper(req$method),
    SERVER_NAME = url$host,
    SERVER_PORT = url$port,
    SERVER_PROTOCOL = paste0("http/", req$http_version),
    SERVER_SOFTWARE = "https://github.com/r-lib/webfakes"
  )
}

split_cgi_output <- function(x) {
  nlnl <- grepRaw("\r?\n\r?\n", x)[1]
  if (is.na(nlnl)) {
    stop("Invalid response from git cgi, no headers?")
  }

  headers <- parse_headers(rawToChar(x[1:(nlnl - 1L)]))

  body <- x[nlnl:length(x)]
  ndrop <- 1L
  while (body[ndrop] != 0x0a) ndrop <- ndrop + 1L
  ndrop <- ndrop + 1L
  while (body[ndrop] != 0x0a) ndrop <- ndrop + 1L
  body <- utils::tail(body, -ndrop)

  list(headers = headers, body = body)
}

parse_status <- function(x) {
  status <- as.integer(strsplit(x, " ", fixed = TRUE)[[1]][1])
  if (is.na(status)) {
    stop("Invalid status from git cgi: ", x)
  }
  status
}

parse_headers <- function (txt) {
  headers <- grep(":", parse_headers0(txt), fixed = TRUE, value = TRUE)
  out <- lapply(headers, split_header)
  names <- tolower(vapply(out, `[[`, character(1), 1))
  values <- lapply(lapply(out, `[[`, 2), trimws)
  names(values) <- names
  values
}

parse_headers0 <- function (txt, multiple = FALSE) {
  if (!length(txt))
    return(NULL)
  if (is.raw(txt)) {
    txt <- rawToChar(txt)
  }
  stopifnot(is.character(txt))
  if (length(txt) > 1) {
    txt <- paste(txt, collapse = "\n")
  }
  sets <- strsplit(txt, "\\r\\n\\r\\n|\\n\\n|\\r\\r")[[1]]
  headers <- strsplit(sets, "\\r\\n|\\n|\\r")
  if (multiple) {
    headers
  }
  else {
    headers[[length(headers)]]
  }
}

split_header <- function(x) {
  pos <- grepRaw(":", x, fixed = TRUE)[1]
  if (is.na(pos)) {
    stop("Invalid response header from git cgi: ", x)
  }
  c(substr(x, 1, pos - 1L), substr(x, pos + 1L, nchar(x)))
}
