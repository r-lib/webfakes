
#' A presser response object
#'
#' presser creates a `presser_response` object for every incoming HTTP
#' request. This object is passed to every matched route and middleware,
#' until the HTTP response is sent. It has reference semantics, so handlers
#' can modify it.
#'
#' Fields and methods:
#'
#' * `app`: The `presser_app` object itself.
#' * `locals`: Local variables, the are shared between the handler
#'   functions. This is for the end user.
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
#' * `send_json(object = NULL, text = NULL, ...)`: Send a JSON response.
#'   Either `object` or `text` must be given. `object` will be converted
#'   to JSON using [jsonlite::toJSON()]. `...` are passed to
#'   [jsonlite::toJSON()]. It sets the content type appropriately.
#' * `send(body)`. Send the specified body. `body` can be a raw vector,
#'   or HTML or other text. For raw vectors it sets the content type to
#'   `application/octet-stream`.
#' * `send_file(path, root = ".")`: Send a file. Set `root = "/"` for
#'   absolute file names. It sets the content type automatically, based
#'   on the extension of the file, if it is not set already.
#' * `send_status(status)`: Send the specified HTTP status code, without
#'   a response body.
#' * `set_header(field, value)`: Set a response header.
#' * `set_status(status)`: Set the response status code.
#' * `set_type(type)`: Set the response content type. If it contains a `/`
#'   character then it is set as is, otherwise it is assumed to be a file
#'   extension, and the corresponding MIME type is set.
#'
#' @seealso [presser_request] for the presser request object.
#' @name presser_response
NULL

new_response <- function(app, api) {
  self <- new_object(
    "presser_response",

    app = app,
    locals = as.environment(as.list(app$locals)),

    get_header = function(field) self$.headers[[tolower(field)]],

    on_response = function(fun) {
      self$.on_response <- c(self$.on_response, list(fun))
      invisible(self)
    },

    redirect = function(path, status = 302) {
      self$
        set_status(status)$
        set_header("location", path)$
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

    send_json = function(object = NULL, text = NULL, ...) {
      if (!is.null(object) && !is.null(text)) {
        stop("Specify only one of `object` and `text` in `send_json()`")
      }

      if (is.null(text)) {
        text <- jsonlite::toJSON(object, ...)
      }

      self$
        set_header("content-type", "application/json")$
        send(text)

      invisible(self)
    },

    send = function(body) {
      self$.body <- body
      invisible(self)
    },

    send_file = function(path, root = ".") {
      self$.body <- c(file = normalizePath(file.path(root, path)))

      # Set content type automatically
      if (is.null(self$get_header("content-type"))) {
        ext <- tools::file_ext(basename(path))
        ct <- mime_find(ext)
        if (!is.na(ct)) {
          self$set_header("content-type", ct)
        }
      }

      invisible(self)
    },

    send_status = function(status) {
      self$
        set_status(status)$
        send("")
      invisible(self)
    },

    set_header = function(field, value) {
      self$.headers[[tolower(field)]] <- value
      invisible(self)
    },

    set_status = function(status) {
      self$.status <- status
      invisible(self)
    },

    set_type = function(type) {
      if (grepl("/", type)) {
        self$set_header("content-type", type)
      } else {
        ct <- mime_find(type)
        if (!is.na(ct)) {
          self$set_header("content-type", ct)
        }
      }
      invisible(self)
    },

    .body = NULL,
    .status = NULL,
    .headers = list(),
    .on_response = NULL
  )

  self
}
