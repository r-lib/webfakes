
test_that("error if cannot start", {
  # does not start before the timeout
  app <- new_app()
  app$listen <- function(...) Sys.sleep(1)
  expect_error(
    new_app_process(app, .process_timeout = 100),
    "presser app subprocess did not start"
  )

  # errors before/while starting
  app <- new_app()
  app$listen <- function(...) stop("oops")
  expect_error(
    new_app_process(app),
    class = "callr_status_error",
    "failed to start presser app process.*oops"
  )

  # sends a different message first
  app <- new_app()
  app$listen <- function(...) "foobar"
  expect_error(
    new_app_process(app),
    "Unexpected message from presser app subprocess"
  )
})

test_that("get_state", {
  app <- new_app()
  on.exit(proc$stop(), add = TRUE)
  proc <- new_app_process(app)
  expect_equal(proc$get_state(), "live")
  proc$.process$kill()
  expect_equal(proc$get_state(), "dead")
  proc$stop()
  expect_equal(proc$get_state(), "not running")
})
