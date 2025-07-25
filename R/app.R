pkg_data <- new.env(parent = emptyenv())

#' Create a new web application
#'
#' @details
#' The typical workflow of creating a web application is:
#'
#' 1. Create a `webfakes_app` object with `new_app()`.
#' 1. Add middleware and/or routes to it.
#' 1. Start is with the `webfakes_app$listen()` method, or start it in
#'    another process with [new_app_process()].
#' 1. Make queries to the web app.
#' 1. Stop it via `CTRL+C` / `ESC`, or, if it is running in another
#'    process, with the `$stop()` method of [new_app_process()].
#'
#' A web application can be
#' * restarted,
#' * saved to disk,
#' * copied to another process using the callr package, or a similar way,
#' * embedded into a package,
#' * extended by simply adding new routes and/or middleware.
#'
#' The webfakes API is very much influenced by the
#' [express.js](https://expressjs.com/) project.
#'
#' ## Create web app objects
#'
#' ```r
#' new_app()
#' ```
#'
#' `new_app()` returns a `webfakes_app` object the has the methods listed
#' on this page.
#'
#' An app is an environment with S3 class `webfakes_app`.
#'
#' ## The handler stack
#'
#' An app has a stack of handlers. Each handler can be a route or
#' middleware. The differences between the two are:
#' * A route is bound to one or more paths on the web server. Middleware
#'   is not (currently) bound to paths, but run for all paths.
#' * A route is usually (but not always) the end of the handler stack for
#'   a request. I.e. a route takes care of sending out the response to
#'   the request. Middleware typically performs some action on the request
#'   or the response, and then the next handler in the stack is invoked.
#'
#' ## Routes
#'
#' The following methods define routes. Each method corresponds to the
#' HTTP verb with the same name, except for `app$all()`, which creates a
#' route for all HTTP methods.
#'
#' ```r
#' app$all(path, ...)
#' app$delete(path, ...)
#' app$get(path, ...)
#' app$head(path, ...)
#' app$patch(path, ...)
#' app$post(path, ...)
#' app$put(path, ...)
#' ... (see list below)
#' ```
#'
#' * `path` is a path specification, see 'Path specification' below.
#' * `...` is one or more handler functions. These will be placed in the
#'   handler stack, and called if they match an incoming HTTP request.
#'   See 'Handler functions' below.
#'
#' webfakes also has methods for the less frequently used HTTP verbs:
#' `CONNECT`, `MKCOL`, `OPTIONS`, `PROPFIND`, `REPORT`. (The method
#' names are always in lowercase.)
#'
#' If a request is not handled by any routes (or handler functions in
#' general), then webfakes will send a simple HTTP 404 response.
#'
#' ## Middleware
#'
#' `app$use()` adds a middleware to the handler stack. A middleware is
#' a handler function, see 'Handler functions' below. webfakes comes with
#' middleware to perform common tasks:
#'
#' * [mw_cookie_parser()] parses `Cookie` headers.
#' * [mw_etag()] adds an `ETag` header to the response.
#' * [mw_json()] parses JSON request bodies.
#' * [mw_log()] logs each requests to standard output, or another connection.
#' * [mw_multipart()] parses multipart request bodies.
#' * [mw_range_parser()] parses `Range` headers.
#' * [mw_raw()] parses raw request bodies.
#' * [mw_static()] serves static files from a directory.
#' * [mw_text()] parses plain text request bodies.
#' * [mw_urlencoded()] parses URL encoded request bodies.
#'
#' ```r
#' app$use(..., .first = FALSE)
#' ```
#'
#' * `...` is a set of (middleware) handler functions. They are added to
#' the handler stack, and called for every HTTP request. (Unless an HTTP
#' response is created before reaching this point in the handler stack.)
#' * `.first` set to `TRUE` is you want to add the handler function
#' to the bottom of the stack.
#'
#' ## Handler functions
#'
#' A handler function is a route or middleware. A handler function is
#' called by webfakes with the incoming HTTP request and the outgoing
#' HTTP response objects (being built) as arguments. The handler function
#' may query and modify the members of the request and/or the response
#' object. If it returns the string `"next"`, then it is _not_ a terminal
#' handler, and once it returns, webfakes will move on to call the next
#' handler in the stack.
#'
#' A typical route:
#'
#' ```r
#' app$get("/user/:id", function(req, res) {
#'   id <- req$params$id
#'   ...
#'   res$
#'     set_status(200L)$
#'     set_header("X-Custom-Header", "foobar")$
#'     send_json(response, auto_unbox = TRUE)
#' })
#' ```
#'
#' * The handler belongs to an API path, which is a wildcard path in
#'   this case. It matches `/user/alice`, `/user/bob`, etc. The handler
#'   will be only called for GET methods and matching API paths.
#' * The handler receives the request (`req`) and the response (`res`).
#' * It sets the HTTP status, additional headers, and sends the data.
#'   (In this case the `webfakes_response$send_json()` method automatically
#'   converts `response` to JSON and sets the `Content-Type` and
#'   `Content-Length` headers.
#' * This is a terminal handler, because it does _not_ return `"next"`.
#'   Once this handler function returns, webfakes will send out the HTTP
#'   response.
#'
#' A typical middleware:
#'
#' ```r
#' app$use(function(req, res) {
#'   ...
#'   "next"
#' })
#' ````
#'
#' * There is no HTTP method and API path here, webfakes will call the
#'   handler for each HTTP request.
#' * This is not a terminal handler, it does return `"next"`, so after it
#'   returns webfakes will look for the next handler in the stack.
#'
#' ## Errors
#'
#' If a handler function throws an error, then the web server will return
#' a HTTP 500 `text/plain` response, with the error message as the
#' response body.
#'
#' ## Request and response objects
#'
#' See [webfakes_request] and [webfakes_response] for the methods of the
#' request and response objects.
#'
#' ## Path specification
#'
#' Routes are associated with one or more API paths. A path specification
#' can be
#'
#' * A "plain" (i.e. without parameters) string. (E.g. `"/list"`.)
#' * A parameterized string. (E.g. `"/user/:id"`.)
#' * A regular expression created via [new_regexp()] function.
#' * A list or character vector of the previous ones. (Regular expressions
#'   must be in a list.)
#'
#' ## Path parameters
#'
#' Paths that are specified as parameterized strings or regular expressions
#' can have parameters.
#'
#' For parameterized strings the keys may contain letters, numbers and
#' underscores. When webfakes matches an API path to a handler with a
#' parameterized string path, the parameters will be added to the
#' request, as `params`. I.e. in the handler function (and subsequent
#' handler functions, if the current one is not terminal), they are
#' available in the `req$params` list.
#'
#' For regular expressions, capture groups are also added as parameters.
#' It is best to use named capture groups, so that the parameters are in
#' a named list.
#'
#' If the path of the handler is a list of parameterized strings or
#' regular expressions, the parameters are set according to the first
#' matching one.
#'
#' ## Templates
#'
#' webfakes supports templates, using any template engine. It comes with
#' a template engine that uses the glue package, see [tmpl_glue()].
#'
#' `app$engine()` registers a template engine, for a certain file
#' extension. The `$render()` method of [webfakes_response]
#' can be called from the handler function to evaluate a template from a
#' file.
#'
#' ```r
#' app$engine(ext, engine)
#' ```
#'
#' * `ext`: the file extension for which the template engine is added.
#'   It should not contain the dot. E.g. `"html"', `"brew"`.
#' * `engine`: the template engine, a function that takes the file path
#'   (`path`) of the template, and a list of local variables (`locals`)
#'   that can be used in the template. It should return the result.
#'
#' An example template engine that uses glue might look like this:
#'
#' ```r
#' app$engine("txt", function(path, locals) {
#'   txt <- readChar(path, nchars = file.size(path))
#'   glue::glue_data(locals, txt)
#' })
#' ```
#'
#' (The built-in [tmpl_glue()] engine has more features.)
#'
#' This template engine can be used in a handler:
#'
#' ```r
#' app$get("/view", function(req, res) {
#'  txt <- res$render("test")
#'  res$
#'    set_type("text/plain")$
#'    send(txt)
#' })
#' ```
#'
#' The location of the templates can be set using the `views` configuration
#' parameter, see the `$set_config()` method below.
#'
#' In the template, the variables passed in as `locals`, and also the
#' response local variables (see `locals` in [webfakes_response]), are
#' available.
#'
#' ## Starting and stopping
#'
#' ```r
#' app$listen(port = NULL, opts = server_opts(), cleanup = TRUE)
#' ```
#'
#' * `port`: port to listen on. When `NULL`, the operating system will
#'   automatically select a free port. Add an `"s"` suffix to the port
#'   to use HTTPS. Use `"0s"` to use an OS assigned port with HTTPS.
#'   See the [how-to] manual page if you want to start the web server
#'   on more than one ports.
#'
#' * `opts`: options to the web server. See [server_opts()] for the
#'   list of options and their default values.
#'
#' * `cleanup`: stop the server (with an error) if the standard input
#'   of the process is closed. This is handy when the app runs in a
#'   `callr::r_session` subprocess, because it stops the app (and the
#'   subprocess) if the main process has terminated.
#'
#' This method does not return, and can be interrupted with `CTRL+C` / `ESC`
#' or a SIGINT signal. See [new_app_process()] for interrupting an app that
#' is running in another process.
#'
#' When `port` is `NULL`, the operating system chooses a port where the
#' app will listen. To be able to get the port number programmatically,
#' before the listen method blocks, it advertises the selected port in a
#' `webfakes_port` condition, so one can catch it:
#'
#' webfakes by default binds only to the loopback interface at 127.0.0.1, so
#' the webfakes web app is never reachable from the network.
#'
#' ```r
#' withCallingHandlers(
#'   app$listen(),
#'   "webfakes_port" = function(msg) print(msg$port)
#' )
#' ```
#'
#' ## Logging
#'
#' webfakes can write an access log that contains an entry for all incoming
#' requests, and also an error log for the errors that happen while
#' the server is running. This is the default behavior for local app
#' (the ones started by `app$listen()` and for remote apps (the ones
#' started via `new_app_process()`:
#'
#' * Local apps do not write an access log by default.
#' * Remote apps write an access log into the
#'   `<tmpdir>/webfakes/<pid>/access.log` file, where `<tmpdir>` is the
#'   session temporary directory of the _main process_, and `<pid>` is
#'   the process id of the _subprocess_.
#' * Local apps write an error log to `<tmpdir>/webfakes/error.log`, where
#'   `<tmpdir>` is the session temporary directory of the current process.
#' * Remote app write an error log to the `<tmpdir>/webfakes/<pid>/error.log`,
#'   where `<tmpdir>` is the session temporary directory of the
#'   _main process_ and `<pid>` is the process id of the _subprocess_`.
#'
#' See [server_opts()] for changing the default logging behavior.
#'
#' ## Shared app data
#'
#' ```r
#' app$locals
#' ```
#'
#' It is often useful to share data between handlers and requests in an
#' app. `app$locals` is an environment that supports this. E.g. a
#' middleware that counts the number of requests can be implemented as:
#'
#' ```
#' app$use(function(req, res) {
#'   locals <- req$app$locals
#'   if (is.null(locals$num)) locals$num <- 0L
#'   locals$num <- locals$num + 1L
#'   "next"
#' })
#' ```
#'
#' [webfakes_response] objects also have a `locals` environment, that is
#' initially populated as a copy of `app$locals`.
#'
#' ## Configuration
#'
#' ```r
#' app$get_config(key)
#' app$set_config(key, value)
#' ```
#'
#' * `key`: configuration key.
#' * `value`: configuration value.
#'
#' Currently used configuration values:
#'
#' * `views`: path where webfakes searches for templates.
#'
#' @return A new `webfakes_app`.
#' @aliases webfakes_app
#' @seealso [webfakes_request] for request objects, [webfakes_response] for
#' response objects.
#' @export
#' @examples
#' # see example web apps in the `/examples` directory in
#' system.file(package = "webfakes", "examples")
#'
#' app <- new_app()
#' app$use(mw_log())
#'
#' app$get("/hello", function(req, res) {
#'   res$send("Hello there!")
#' })
#'
#' app$get(new_regexp("^/hi(/.*)?$"), function(req, res) {
#'   res$send("Hi indeed!")
#' })
#'
#' app$post("/hello", function(req, res) {
#'   res$send("Got it, thanks!")
#' })
#'
#' app
#'
#' # Start the app with: app$listen()
#' # Or start it in another R session: new_app_process(app)

new_app <- function() {
  self <- new_object(
    "webfakes_app",

    all = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("all", path, ...))
      invisible(self)
    },

    connect = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("connect", path, ...))
      invisible(self)
    },

    delete = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("delete", path, ...))
      invisible(self)
    },

    get = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("get", path, ...))
      invisible(self)
    },

    engine = function(ext, engine) {
      rec <- list(ext = ext, engine = engine)
      self$.engines <- c(self$.engines, list(rec))
      invisible(self)
    },

    get_config = function(key) {
      self$.config[[key]]
    },

    head = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("head", path, ...))
      invisible(self)
    },

    listen = function(port = NULL, opts = server_opts(), cleanup = TRUE) {
      if (is_na_scalar(port)) {
        port <- NULL
      }
      opts$port <- port
      self$.enable_keep_alive <- opts$enable_keep_alive
      opts$access_log_file <- sub("%p", Sys.getpid(), opts$access_log_file)
      opts$error_log_file <- sub("%p", Sys.getpid(), opts$error_log_file)
      self$.opts <- opts

      tryCatch(srv <- server_start(opts), error = function(err) {
        err$message <- paste(sep = "\n", err$message, self$.get_error_log())
        stop(err)
      })
      self$.ports <- server_get_ports(srv)
      self$.port <- self$.ports$port[1]
      port_nums <- paste0(
        self$.ports$port,
        ifelse(self$.ports$ssl, " (SSL)", "")
      )
      message(
        "Running webfakes web app on port",
        if (length(port_nums) > 1) "s " else " ",
        paste(port_nums, collapse = ", ")
      )
      if (!is.na(opts$access_log_file)) {
        message("Access log file: ", opts$access_log_file)
      }
      if (!is.na(opts$error_log_file)) {
        message("Error log file: ", opts$error_log_file)
      }
      msg <- structure(
        list(
          ports = self$.ports,
          port = self$.port,
          access_log = attr(srv, "options")$access_log_file,
          error_log = attr(srv, "options")$error_log_file
        ),
        class = c("webfakes_port", "callr_message", "condition")
      )
      message(msg)

      on.exit(server_stop(srv), add = TRUE)

      while (TRUE) {
        req <- server_poll(srv, cleanup)
        tryCatch(
          self$.process_request(req),
          error = function(err) {
            cat(as.character(err), file = stderr())
            response_send_error(req, as.character(err), 500L)
          }
        )
      }
    },

    mkcol = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("mkcol", path, ...))
      invisible(self)
    },

    options = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("options", path, ...))
      invisible(self)
    },

    patch = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("patch", path, ...))
      invisible(self)
    },

    post = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("post", path, ...))
      invisible(self)
    },

    propfind = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("propfind", path, ...))
      invisible(self)
    },

    put = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("put", path, ...))
      invisible(self)
    },

    report = function(path, ...) {
      self$.stack <- c(self$.stack, parse_handlers("report", path, ...))
      invisible(self)
    },

    set_config = function(key, value) {
      self$.config[[key]] <- value
      invisible(self)
    },

    use = function(..., .first = FALSE) {
      mw <- parse_handlers("use", "*", ...)
      if (.first) {
        self$.stack <- c(mw, self$.stack)
      } else {
        self$.stack <- c(self$.stack, mw)
      }
      invisible(self)
    },

    # Public data to be used in handlers
    locals = new.env(parent = parent.frame()),

    # Private data
    .ports = NULL,
    .port = NULL,
    .enable_keep_alive = NULL,
    .opts = NULL,

    # middleware stack
    .stack = list(),

    # view engines
    .engines = list(),

    # config
    .config = as.environment(list(
      views = file.path(getwd(), "views")
    )),

    # The request processing function
    .process_request = function(req) {
      req <- new_request(self, req)
      res <- req$res
      res$.delay <- NULL

      tryCatch(
        {
          for (i in sseq(res$.stackptr, length(self$.stack))) {
            handler <- self$.stack[[i]]
            m <- path_match(req$method, req$path, handler)
            if (!isFALSE(m)) {
              res$.i <- i
              if (is.list(m)) {
                req$params <- m$params
              }
              out <- handler$handler(req, res)
              if (!identical(out, "next")) break
            }
          }

          if (!res$.sent && is.null(res$.delay)) {
            if (!res$headers_sent) {
              res$send_status(404)
            } else if (
              (res$get_header("Transfer-Encoding") %||% "") == "chunked"
            ) {
              res$send_chunk(raw(0))
              res$headers_sent <- TRUE
              res$send("")
            } else {
              res$send("")
            }
          }
        },
        webfakes_error = function(err) {}
      )
    },

    .get_error_log = function() {
      if (!is.na(self$.opts$error_log_file)) {
        paste0("Error log:\n", read_char(self$.opts$error_log_file))
      }
    }
  )

  self
}

parse_handlers <- function(method, path, ...) {
  handlers <- list(...)
  ans <- list()
  for (h in seq_along(handlers)) {
    handler <- handlers[[h]]
    if (is.function(handler)) {
      rec <- list(
        method = method,
        path = path,
        handler = handler,
        name = names(handlers)[h]
      )
      ans <- c(ans, list(rec))
    } else {
      stop("Invalid webfakes handler")
    }
  }

  ans
}
