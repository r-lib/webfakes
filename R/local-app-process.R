
#' @export

local_app_process <- function(app, ..., .env = parent.frame(),
                              .teardown_env = parent.frame()) {

  proc <- do.call("new_app_process", list(app, ...), envir = .env)
  withr::defer(proc$stop(), envir = .teardown_env)
  proc
}
