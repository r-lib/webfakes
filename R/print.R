format_named_list <- function(name, data) {
  c(paste0(name, ":"), if (length(data)) paste0("  ", names(data), ": ", data))
}

format_path <- function(patterns) {
  # Make sure patterns is a list
  if (inherits(patterns, "webfakes_regexp")) {
    patterns <- list(patterns)
  } else if (is.character(patterns)) {
    patterns <- as.list(patterns)
  }
  paste(collapse = ", ", vapply(patterns, format, character(1)))
}

#' @export

format.webfakes_app <- function(x, ...) {
  header <- "<webfakes_app>"
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
  help <- "# see ?webfakes_app for all methods"
  c(header, "routes:", data, "fields and methods:", methods, help)
}

#' @export

print.webfakes_app <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.webfakes_request <- function(x, ...) {
  header <- "<webfakes_request>"
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
    "  app                    # the webfakes_app the request belongs to",
    "  headers                # HTTP request headers",
    "  hostname               # server hostname, the Host header",
    "  method                 # HTTP method of request (lowercase)",
    "  path                   # server path",
    "  protocol               # http or https",
    "  query_string           # raw query string without '?'",
    "  query                  # named list of query parameters",
    "  remote_addr            # IP address of the client",
    "  url                    # full URL of the request",
    "  get_header(field)      # get a request header"
  )
  help <- " # see ?webfakes_request for details"
  c(header, data, "fields and methods:", methods, help)
}

#' @export

print.webfakes_request <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.webfakes_response <- function(x, ...) {
  header <- "<webfakes_reeponse>"
  methods <- c(
    "  app                    # the webfakes_app the response belongs to",
    "  locals                 # response-wide shared data",
    "  get_header(field)      # query response header",
    "  on_response(fun)       # call handler function for complete response",
    "  redirect(path, status) # send redirect response",
    "  render(view, locals)   # render template",
    "  send(body)             # send text or raw data",
    "  send_file(path, root)  # send a file (automatic Content-Type)",
    "  send_json(object, text, ...)",
    "                         # send JSON data",
    "  send_status(status)    # send HTTP status and empty body",
    "  set_header(field, value)
                              # set a response header",
    "  set_status(status)     # set response status code",
    "  set_type(type)         # set Content-Type"
  )
  help <- " # see ?webfakes_response for details"
  c(header, "fields and methods:", methods, help)
}

#' @export

print.webfakes_response <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.webfakes_regexp <- function(x, ...) {
  paste0("<webfakes_regexp> ", encodeString(x, quote = "\""))
}

#' @export

print.webfakes_regexp <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.webfakes_app_process <- function(x, ...) {
  header <- "<webfakes_app_process>"
  state <- x$get_state()
  data <- c(
    "state:",
    paste0("  ", state),
    "auto_start:",
    paste0("  ", x$.auto_start),
    "process id:",
    paste0("  ", if (state == "not running") "none" else x$.process$get_pid()),
    "http url:",
    paste0("  ", if (state == "live") x$url() else "NA")
  )
  methods <- c(
    "  get_app()              # get the app object",
    "  get_port()             # query (first) port of the app",
    "  get_ports()            # query all ports of the app",
    "  get_state()            # query web server process state",
    "  local_env(envvars)     # set temporary environment variables",
    "  start()                # start the app",
    "  url(path, query)       # query url for an api path",
    "  stop()                 # stop web server process"
  )
  help <- "# see ?webfakes_app_process for details"
  c(header, data, "fields and methods:", methods, help)
}

#' @export

print.webfakes_app_process <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
