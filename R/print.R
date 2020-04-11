
format_named_list <- function(name, data) {
  c(paste0(name, ":"),
    if (length(data)) paste0("  ", names(data), ": ", data)
  )
}

format_path <- function(patterns) {
  # Make sure patterns is a list
  if (inherits(patterns, "presser_regexp")) {
    patterns <- list(patterns)
  } else if (is.character(patterns)) {
    patterns <- as.list(patterns)
  }
  paste(collapse = ", ", vapply(patterns, format, character(1)))
}

#' @export

format.presser_app <- function(x, ...) {
  header <- "<presser_app>"
  data <- vapply(x$.stack, FUN.VALUE = character(1), function(x) {
    name <- if (nchar(x$name %||% "")) paste0(" # ", x$name)
    paste0("  ", x$method, " ", format_path(x$path), name)
  })
  methods <- c(
    "  all(path, ...)         # add route for *all* HTTP methods",
    "  delete(path, ...)      # add route for DELETE",
    "  engine(ext, engine)    # add template engine for file extension",
    "  head(path, ...)        # add route for HEAD",
    "  listen(port)           # start web app on port",
    "  patch(path, ...)       # add route for PATCH",
    "  post(path, ...)        # add route for POST",
    "  put(path, ...)         # add route for PUT",
    "  use(...)               # add middleware",
    "  locals                 # app-wide shared data"
  )
  help <- "# see ?presser_app for all methods"
  c(header, "routes:", data, "fields and methods:", methods, help)
}

#' @export

print.presser_app <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.presser_request <- function(x, ...) {
  header <- "<presser_request>"
  data <- c(
    "method:",
    paste0("  ", x$method),
    "url:",
    paste0("  ", x$url),
    "client:",
    paste0("  ", x$remote_addr),
    format_named_list("query", x$query),
    format_named_list("headers", x$headers)
  )
  methods <- c(
    "  app                    # the presser_app the request belongs to",
    "  headers                # HTTP request headers",
    "  hostname               # server hostname, the Host header",
    "  method                 # HTTP method of request (lowercase)",
    "  path                   # server path",
    "  protocol               # http or https",
    "  query_string           # raw query string with '?'",
    "  query                  # named list of query parameters",
    "  remote_addr            # IP address of the client",
    "  url                    # full URL of the request",
    "  get_header(field)      # get a request header"
  )
  help <- " # see ?presser_request for details)"
  c(header, data, "fields and methods:", methods, help)
}

#' @export

print.presser_request <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.presser_response <- function(x, ...) {
  header <- "<presser_reeponse>"
  methods <- c(
    "  app                    # the presser_app the response belongs to",
    "  locals                 # response-wide shared data",
    "  get_header(field)      # query response header",
    "  on_response(fun)       # call handler function for complete response",
    "  redirect(path, status) # send redirect response",
    "  render(view, locals)   # render template",
    "  send(body)             # send text or raw data",
    "  send_file(path, root)  # send a file (automatic content-type)",
    "  send_json(object, text, ...)",
    "                         # send JSON data",
    "  send_status(status)    # send HTTP status and empty body",
    "  set_header(field, value)
                              # set a response header",
    "  set_status(status)     # set response status code",
    "  set_type(type)         # set content-type"
  )
  help <- " # see ?presser_response for details)"
  c(header, "fields and methods:", methods, help)
}

#' @export

print.presser_response <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.presser_regexp <- function(x, ...) {
  paste0("<presser_regexp> ", encodeString(x, quote = "\""))
}

#' @export

print.presser_regexp <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.presser_app_process <- function(x, ...) {
  header <- "<presser_app_process>"
  data <- c(
    "state:",
    paste0("  ", x$get_state()),
    "process id:",
    paste0("  ", if (is.null(x$.process)) "none" else x$.process$get_pid()),
    "http url:",
    paste0("  ", x$get_url())
  )
  methods <- c(
    "  get_port()             # query port of the app",
    "  get_state()            # query web server process state",
    "  get_url(path, query)   # query url for an api path",
    "  stop()                 # stop web server process"
  )
  help <- "# see ?presser_app_process for details)"
  c(header, data, "fields and methods:", methods, help)
}

#' @export

print.presser_app_process <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
