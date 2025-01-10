
#' A webfakes response object
#'
#' webfakes creates a `webfakes_response` object for every incoming HTTP
#' request. This object is passed to every matched route and middleware,
#' until the HTTP response is sent. It has reference semantics, so handlers
#' can modify it.
#'
#' Fields and methods:
#'
#' * `app`: The `webfakes_app` object itself.
#' * `req`: The request object.
#' * `headers_sent`: Whether the response headers were already sent out.
#' * `locals`: Local variables, the are shared between the handler
#'   functions. This is for the end user, and not for the middlewares.
#' * `delay(secs)`: delay the response for a number of seconds. If a
#'   handler calls `delay()`, the same handler will be called again,
#'   after the specified number of seconds have passed. Use the `locals`
#'   environment to distinguish between the calls. If you are using
#'   `delay()`, and want to serve requests in parallel, then you probably
#'   need a multi-threaded server, see [server_opts()].
#' * `add_header(field, value)`: Add a response header. Note that
#'   `add_header()` may create duplicate headers. You usually want
#'   `set_header()`.
#' * `get_header(field)`: Query the currently set response headers. If
#'   `field` is not present it return `NULL`.
#' * `on_response(fun)`: Run the `fun` handler function just before the
#'   response is sent out. At this point the headers and the body are
#'   already properly set.
#' * `redirect(path, status = 302)`: Send a redirect response. It sets
#'   the `Location` header, and also sends a `text/plain` body.
#' * `render(view, locals = list())`: Render a template page. Searches
#'   for the `view` template page, using all registered engine extensions,
#'   and calls the first matching template engine. Returns the filled
#'   template.
#' * `send(body)`. Send the specified body. `body` can be a raw vector,
#'   or HTML or other text. For raw vectors it sets the content type to
#'   `application/octet-stream`.
#' * `send_json(object = NULL, text = NULL, ...)`: Send a JSON response.
#'   Either `object` or `text` must be given. `object` will be converted
#'   to JSON using [jsonlite::toJSON()]. `...` are passed to
#'   [jsonlite::toJSON()]. It sets the content type appropriately.
#' * `send_file(path, root = ".")`: Send a file. Set `root = "/"` for
#'   absolute file names. It sets the content type automatically, based
#'   on the extension of the file, if it is not set already.
#' * `send_status(status)`: Send the specified HTTP status code, without
#'   a response body.
#' * `send_chunk(data)`: Send a chunk of a response in chunked encoding.
#'   The first chunk will automatically send the HTTP response headers.
#'   Webfakes will automatically send a final zero-lengh chunk, unless
#'   `$delay()` is called.
#' * `set_header(field, value)`: Set a response header. If the headers have
#'   been sent out already, then it throws a warning, and does nothing.
#' * `set_status(status)`: Set the response status code. If the headers
#'   have been sent out already, then it throws a warning, and does nothing.
#' * `set_type(type)`: Set the response content type. If it contains a `/`
#'   character then it is set as is, otherwise it is assumed to be a file
#'   extension, and the corresponding MIME type is set. If the headers have
#'   been sent out already, then it throws a warning, and does nothing.
#' * `add_cookie(name, value, options)`: Adds a cookie to the response.
#'   `options` is a named list, and may contain:
#'    * `domain`: Domain name for the cookie, not set by default.
#'    * `expires`: Expiry date in GMT. It must be a POSIXct object, and
#'       will be formatted correctly.
#'    * 'http_only': if TRUE, then it sets the 'HttpOnly' attribute, so
#'      Javasctipt cannot access the cookie.
#'    * `max_age`: Maximum age, in number of seconds.
#'    * `path`: Path for the cookie, defaults to "/".
#'    * `same_site`: The 'SameSite' cookie attribute. Possible values are
#'      "strict", "lax" and "none".
#'    * `secure`: if TRUE, then it sets the 'Secure' attribute.
#' * `clear_cookie(name, options = list())`: clears a cookie. Typically,
#'    web browsers will only clear a cookie if the options also match.
#' * `write(data)`: writes (part of) the body of the response. It also
#'   sends out the response headers, if they haven't been sent out before.
#'
#' Usually you need one of the `send()` methods, to send out the HTTP
#' response in one go, first the headers, then the body.
#'
#' Alternatively, you can use `$write()` to send the response in parts.
#'
#' @seealso [webfakes_request] for the webfakes request object.
#' @name webfakes_response
#' @examples
#' # This is how you can see the request and response objects:
#' app <- new_app()
#' app$get("/", function(req, res) {
#'   browser()
#'   res$send("done")
#' })
#' app
#'
#' # Now start this app on a port:
#' # app$listen(3000)
#' # and connect to it from a web browser: http://127.0.0.1:3000
#' # You can also use another R session to connect:
#' # httr::GET("http://127.0.0.1:3000")
#' # or the command line curl tool:
#' # curl -v http://127.0.0.1:3000
#' # The app will stop while processing the request.
NULL

new_response <- function(app, req) {
  self <- new_object(
    "webfakes_response",

    app = app,
    req = req,
    locals = as.environment(as.list(app$locals)),
    headers_sent = FALSE,

    delay = function(secs) {
      self$.stackptr <- self$.i
      self$.delay <- secs
      response_delay(self$req, secs)
      invisible(NULL)
    },

    get_header = function(field) {
      # this is case insensitive
      h <- self$.headers
      names(h) <- tolower(names(h))
      h[[tolower(field)]]
    },

    on_response = function(fun) {
      self$.on_response <- c(self$.on_response, list(fun))
      invisible(self)
    },

    redirect = function(path, status = 302) {
      if (self$.check_sent()) return(invisible(self))
      self$
        set_status(status)$
        set_header("Location", path)$
        set_type("text/plain")$
        send(paste0(
          status, " ", http_statuses[as.character(status)],
          ". Redirecting to ", path
        ))
      invisible(self)
    },

    render = function(view, locals = list()) {
      locals <- as.environment(as.list(locals))
      parent.env(locals) <- self$locals
      root <- self$app$get_config("views")
      for (eng in self$app$.engines) {
        f <- file.path(root, paste0(view, ".", eng$ext))
        if (file.exists(f)) return(eng$engine(f, locals))
      }
      stop("Cannot find template engine for view '", view, "'")
    },

    send = function(body) {
      if (self$.check_sent()) return(invisible(self))
      # We need to do these here, on_response middleware might depend on it
      self$.body <- body
      self$.set_defaults()
      for (fn in self$.on_response) fn(self$req, self)

      response_send(self$req)

      self$headers_sent <- TRUE
      self$.sent <- TRUE
      invisible(self)
    },

    send_chunk = function(data) {
      if (self$.check_sent()) return(invisible(self))
      # The first chunk sends the headers automatically, but we make
      # sure to set chunked encoding
      if (! self$headers_sent) {
        self$set_header("Transfer-Encoding", "chunked")
        if (is.null(self$get_header("Content-Type"))) {
          self$set_header("Content-Type", "application/octet-stream")
        }
        if (is.null(self$.status)) self$set_status(200L)
        self$.set_defaults()
      }
      enc <- self$get_header("Transfer-Encoding")
      if (enc != "chunked") {
        warning("Headers sent, cannot set chunked encoding now")
        return(invisible(self))
      }
      if (is.character(data)) data <- charToRaw(paste(data, collapse = "\n"))
      response_send_chunk(self$req, data)
      self$headers_sent <- TRUE
      invisible(self)
    },

    send_json = function(object = NULL, text = NULL, ...) {
      if (!is.null(object) && !is.null(text)) {
        stop("Specify only one of `object` and `text` in `send_json()`")
      }

      if (is.null(text)) {
        text <- jsonlite::toJSON(object, ...)
      }

      self$
        set_header("Content-Type", "application/json")$
        send(text)
    },

    send_file = function(path, root = ".") {
      # Set content type automatically
      if (is.null(self$get_header("Content-Type"))) {
        ext <- tools::file_ext(basename(path))
        ct <- mime_find(ext)
        if (!is.na(ct)) {
          self$set_header("Content-Type", ct)
        }
      }

      if (root == "/" && .Platform$OS.type == "windows" &&
          grepl("^[a-zA-Z]:", path)) {
        abs_path <- path
      } else {
        abs_path <- file.path(root, path)
      }

      self$send(read_bin(normalizePath(abs_path)))
    },

    send_status = function(status) {
      self$
        set_status(status)$
        send("")
    },

    set_header = function(field, value) {
      if (self$.check_sent()) return(invisible(self))
      self$.headers[[field]] <- as.character(value)
      invisible(self)
    },

    add_header = function(field, value) {
      if (self$.check_sent()) return(invisible(self))
      h <- structure(list(value), names = field)
      self$.headers <- append(self$.headers, h)
      invisible(self)
    },

    set_status = function(status) {
      if (self$.check_sent()) return(invisible(self))
      self$.status <- as.integer(status)
      invisible(self)
    },

    set_type = function(type) {
      if (self$.check_sent()) return(invisible(self))
      if (grepl("/", type)) {
        self$set_header("Content-Type", type)
      } else {
        ct <- mime_find(type)
        if (!is.na(ct)) {
          self$set_header("Content-Type", ct)
        }
      }
      invisible(self)
    },

    add_cookie = function(name, value, options = list()) {
      if (!is_string(name)) {
        stop("Cookie name must be a string.")
      }
      if (grepl("[=;]", name)) {
        stop("Cookie name cannot contain ';' and '=' characters.")
      }
      if (!is_string(value)) {
        stop("Cookie value must be a string.")
      }
      if (grepl("[=;]", value)) {
        stop("Cookie value cannot contain ';' and '=' characters.")
      }

      ck <- paste0(
        name, "=", value,
        "; ",
        format_cookie_options(options)
      )
      self$add_header("Set-Cookie", ck)
      invisible(self)
    },

    clear_cookie = function(name, options = list()) {
      if (!is_string(name)) {
        stop("Cookie name must be a string.")
      }
      if (grepl("[=;]", name)) {
        stop("Cookie name cannot contain ';' and '=' characters.")
      }

      options$expires <- .POSIXct(0)
      options$max_age <- 0L
      ck <- paste0(name, "=; ", format_cookie_options(options))
      self$add_header("Set-Cookie", ck)
      invisible(self)
    },

    write = function(data) {
      if (is.null(self$get_header("content-length"))) {
        warning("response$write() without a Content-Length header")
      }
      if (is.null(self$.status)) self$set_status(200L)
      if (is.character(data)) data <- charToRaw(paste(data, collapse = "\n"))
      response_write(self$req, data)
      self$headers_sent <- TRUE
      invisible(self)
    },

    .check_sent = function() {
      if (isTRUE(self$.sent)) {
        warning("Response is sent already")
      }
      self$.sent
    },

    .set_defaults = function() {

      if (is.null(self$.status)) {
        if (is.null(self$.body)) {
          # No status, no body, that's 404
          self$.status <- 404L
          self$.body <- "Not found"
        } else {
          # No status, but body, set status
          self$.status <- 200L
        }
      }

      # Set Content-Type if not set
      if (is.null(self$get_header("Content-Type"))) {
        if (is.raw(self$body)) {
          ct <- "application/octet-stream"
        } else {
          ct <- "text/plain"
        }
        self$set_header("Content-Type", ct)
      }

      # Set Content-Length if not set
      if (is.null(self$get_header("Content-Length")) &&
          (self$get_header("Transfer-Encoding") %||% "") != "chunked") {
        if (is.raw(self$.body)) {
          cl <- length(self$.body)
        } else if (is.character(self$.body)) {
          cl <- nchar(self$.body, type = "bytes")
        } else if (is.null(self$.body)) {
          cl <- 0L
        }
        self$set_header("Content-Length", cl)
      }

      # Make sure response to HEAD has empty body
      if (self$req$method == "head") {
        self$.body <- raw(0)
        self$set_header("Content-Length", "0")
      }
    },

    .body = NULL,
    .status = NULL,
    .headers = if (!app$.enable_keep_alive) list("Connection" = "close") else list(),
    .on_response = NULL,
    .sent = FALSE,
    .stackptr = 1L
  )

  self
}

format_cookie_options <- function(options) {
  options$path <- options$path %||% "/"

  bad <- unique(setdiff(
    names(options),
    c("domain", "expires", "http_only", "max_age", "path", "same_site",
      "secure")
  ))
  if (length(bad)) {
    stop(
      "Unknown or unsupported cookie attribute(s): ",
      paste0("\"", bad, "\"", collapse = ", "),
      "."
    )
  }

  parts <- c(

    if (!is.null(options$domain)) {
      paste0("Domain=", options$domain)
    },

    if (!is.null(options$expires)) {
      if (!inherits(options$expires, "POSIXct")) {
        stop("The 'expires' cookie attribute must be a POSIXct object")
      }
      paste0("Expires=", http_time_stamp(options$expires))
    },

    if (isTRUE(options$http_only)) {
      "HttpOnly"
    },

    if (!is.null(options$max_age)) {
      paste0("Max-Age=", options$max_age)
    },

    paste0("Path=", options$path),

    if (!is.null(options$same_site)) {
      if (tolower(!options$same_site) %in% c("strict", "lax", "none")) {
        stop(
          "Invalid value for 'SameSite' cookie atrribute: ",
          options$same_site,
          ", must be \"strict\", \"lax\" or \"none\"."
        )
      }
      paste0("SameSite=", capitalize(options$same_site))
    },

    if (isTRUE(options$secure)) {
      "Secure"
    }
  )

  paste(parts, collapse = "; ")
}
