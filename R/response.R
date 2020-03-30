
new_response <- function(app, api) {
  self <- new_object(
    "press_response",

    app = app,
    locals = new.env(parent = app$locals),

    append_header = function(field, value) { stop("TODO") },
    set_cookie = function(name, value, ...) { stop("TODO") },
    clear_cookie = function(name) { stop("TODO") },
    download = function(path, filename = basename(path), ...) { stop("TODO") },
    get_header = function(field) self$.headers[[tolower(field)]],

    on_response = function(fun) {
      self$.on_response <- c(self$.on_response, list(fun))
      invisible(self)
    },

    redirect = function(path, status = 302) { stop("TODO") },

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

    send_json = function(object, text) { stop("TODO") },

    send = function(body) self$.body <- body,

    send_file = function(path, ...) { stop("TODO") },
    send_status = function(status) { stop("TODO") },

    set_header = function(field, value) {
      self$.headers[[tolower(field)]] <- value
      invisible(self)
    },

    set_status = function(status) {
      self$.status <- status
      invisible(self)
    },

    set_type = function(type) { stop("TODO") },

    .body = NULL,
    .status = NULL,
    .headers = NULL,
    .on_response = NULL
  )

  self
}
