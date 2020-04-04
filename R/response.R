
new_response <- function(app, api) {
  self <- new_object(
    "press_response",

    app = app,
    locals = as.environment(as.list(app$locals)),

    # append_header = function(field, value) { stop("TODO") },
    # set_cookie = function(name, value, ...) { stop("TODO") },
    # clear_cookie = function(name) { stop("TODO") },
    # download = function(path, filename = basename(path), ...) { stop("TODO") },
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

    send_file = function(path, max_age = NA, root = ".",
                         last_modified = TRUE, headers = NULL,
                         dotfiles = "ignore",
                         cache_control = TRUE,
                         immutable = FALSE) {
      # TODO: implement options
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
