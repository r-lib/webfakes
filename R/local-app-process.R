
#' App process that is cleaned up automatically
#'
#' You can start the process with an explicit `$start()` call.
#' Alternatively it starts up at the first `$url()` or `$get_port()`
#' call.
#'
#' @inheritParams new_app_process
#' @param ... Passed to [new_app_process()].
#' @param .local_envir The environment to attach the process cleanup to.
#'   Typically a frame. When this frame finishes, the process is stopped.
#'
#' @export
#' @seealso [new_app_process()] for more details.

local_app_process <- function(app, ..., .local_envir = parent.frame()) {
  proc <- new_app_process(app, ...)
  withr::defer(proc$stop(), envir = .local_envir)
  proc
}
