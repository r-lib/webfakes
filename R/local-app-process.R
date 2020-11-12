
#' App process that is cleaned up automatically
#'
#' You can start the process with an explicit `$start()` call.
#' Alternatively it starts up at the first `$url()` or `$get_port()`
#' call.
#'
#' @inheritParams new_app_process
#' @param ... Passed to [new_app_process()].
#' @param .env The environment to run [new_app_process()] in.
#' @param .teardown_env The environment to attach the process cleanup to.
#'   Typically a frame. When this frame finishes, the process is stopped.
#'
#' @export
#' @seealso [new_app_process()] for more details.

local_app_process <- function(app, ..., .env = parent.frame(),
                              .teardown_env = parent.frame()) {

  proc <- do.call("new_app_process", list(app, ...), envir = .env)
  withr::defer(proc$stop(), envir = .teardown_env)
  proc
}
