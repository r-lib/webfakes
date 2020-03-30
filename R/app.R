
pkg_data <- new.env(parent = emptyenv())

#' Create a new web application
#'
#' It's API:
#'
#' ```r
#' app <- new_app()
#' ```
#'
#' ```r
#' app$get_config(key)
#' app$set_config(key, value)
#' app$path()
#' ```
#'
#' ```r
#' app$all(path, ...)
#' app$get(path, ...)
#' app$post(path, ...)
#' app$use(..., path = "/")
#' ```
#'
#' ```r
#' app$engine(ext, engine)
#' ```
#'
#' ```r
#' app$locals
#' ```
#'
#' ```r
#' app$listen(port = NULL)
#' ```
#' @name app
#' @export

new_app <- function() {

  self <- new_object(
    "pressr_app",

    all = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("all", path, ...))
      invisible(self)
    },

    engine = function(ext, engine) {
      rec <- list(ext = ext, engine = engine)
      self$.engines <- c(self$.engines, list(rec))
      invisible(self)
    },

    get = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("get", path, ...))
      invisible(self)
    },

    get_config = function(key) {
      self$.config[[key]]
    },

    get_port = function() {
      self$.port
    },

    listen = function(port = NULL)  {
      stopifnot(is.null(port) || is_port(port) || is_na_scalar(port))
      if (is_na_scalar(port)) port <- NULL

      try(tools::startDynamicHelp(FALSE), silent = TRUE)
      Sys.unsetenv("R_DISABLE_HTTPD")

      # NULL or a real port number
      options(help.ports = port)

      do.call(paste0("unlock", "Binding"), list("httpd", asNamespace("tools")))
      pkg_data$tools_httpd <- asNamespace("tools")$httpd
      on.exit({
        assign("httpd", pkg_data$tools_httpd, envir = asNamespace("tools"))
        try(tools::startDynamicHelp(FALSE), silent = TRUE)
        self$.port <- NULL
      }, add = TRUE)
      assign("httpd", self$.run, envir = asNamespace("tools"))

      port2 <- suppressMessages(tools::startDynamicHelp(TRUE))

      if (!is.null(port) && port != port2) {
        stop("Cannot start server on port ", port)
      }
      self$.port <- port2

      message("Running pressr web app on port ", self$.port)
      while (TRUE) Sys.sleep(1000)
    },

    path = function() {
      self$.mountpath
    },

    post = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("post", path, ...))
      invisible(self)
    },

    set_config = function(key, value) {
      self$.config[[key]] <- value
      invisible(self)
    },

    use = function(..., path = "/") {
      self$.stack <- c(self$.stack, parse_handlers("use", path, ...))
      invisible(self)
    },

    # Public data to be used in handlers
    locals = new.env(parent = parent.frame()),

    # Private data
    .mountpath = NULL,
    .port = NULL,

    # middleware stack
    .stack = list(),

    # view engines
    .engines = list(),

    # config
    .config = as.environment(list(
      views = file.path(getwd(), "views")
    )),

    # The request processing function
    .run = function(path, query, body, headers) {

      req <- new_request(self, path, query, body, headers)
      res <- new_response(self)

      for (h in self$.stack) {
        m <- path_match(req$method, path, h)
        if (!isFALSE(m)) {
          if (is.list(m)) req$params <- m$params
          out <- h$handler(req, res)
          if (!identical(out, "next")) break
        }
      }

      content_type <- res$.headers[["content-type"]] %||% "text/plain"
      res$.headers <- res$.headers[names(res$.headers) != "content-type"]

      if (is.null(res$.status)) {
        res$.status <- if (is.null(res$.body)) 404L else 200L
      }
      if (is.null(res$.body)) {
        res$.body <- "Not found"
      }

      on.exit(for (fn in res$.on_response) try(fn(req, res)), add = TRUE)

      list(
        res$.body,
        content_type,
        if (length(res$headers)) {
          paste0(names(res$.headers), ": ", res$.headers)
        },
        res$.status
      )
    }
  )

  self
}

parse_handlers <- function(method, path, ...) {
  handlers <- list(...)
  ans <- list()
  for (h in handlers) {
    if (is.function(h)) {
      rec <- list(method = method, path = path, handler = h)
      ans <- c(ans, list(rec))
    } else {
      stop("Invalid pressr handler")
    }
  }

  ans
}
