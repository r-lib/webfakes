
#' @export

new_app_process <- function(app) {

  self <- new_object(
    "pressr_app_process",

    new = function(app) {
      self$.app <- app
      self$.process <- callr::r_session$new(wait = TRUE)
      self$.port <- self$.process$run(
        args = list(app),
        function(app) {
          library(pressr)
          .GlobalEnv$app <- app
          app$listen(port = NULL, block = FALSE)
          app$get_port()
        }
      )

      self$.process$call(
        function() while(TRUE) Sys.sleep(1000)
      )

      invisible(self)
    },

    get_url = function(...) {
      paste(
        c(paste0("http://127.0.0.1:", self$.port), ...),
        collapse = "/"
      )
    },

    get_port = function() self$.port,

    stop = function() {
      self$.process$kill()
      self$.process <- NULL
      invisible(self)
    },

    get_state = function() {
      if (is.null(self$.process)) {
        "not running"
      } else if (self$.process$is_alive()) {
        "live"
      } else {
        "dead"
      }
    },

    .process = NULL,
    .app = NULL,
    .port = NULL
  )

  self$new(app)
  self$new <- NULL

  self
}
