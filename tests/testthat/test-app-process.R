
test_that("error if cannot start", {
  # does not start before the timeout
  app <- new_app()
  app$listen <- function(...) Sys.sleep(1)
  expect_error(
    new_app_process(app, process_timeout = 100, start = TRUE),
    "presser app subprocess did not start"
  )

  # errors before/while starting
  app <- new_app()
  app$listen <- function(...) stop("oops")
  expect_error(
    new_app_process(app, start = TRUE),
    class = "callr_status_error",
    "failed to start presser app process.*oops"
  )

  # sends a different message first
  app <- new_app()
  app$listen <- function(...) "foobar"
  expect_error(
    new_app_process(app, start = TRUE),
    "Unexpected message from presser app subprocess"
  )
})

test_that("get_state", {
  app <- new_app()
  on.exit(proc$stop(), add = TRUE)
  proc <- new_app_process(app, start = TRUE)
  expect_equal(proc$get_state(), "live")
  proc$.process$kill()
  expect_equal(proc$get_state(), "dead")
  expect_output(proc$stop(), "presser process dead")
  expect_equal(proc$get_state(), "not running")
})

test_that("env vars", {
  app <- new_app()
  on.exit(proc$stop(), add = TRUE)
  withr::local_envvar(list(FOO = "foo"))
  proc <- new_app_process(app, start = TRUE)
  proc$local_env(list(FOO = "bar"))
  expect_equal(Sys.getenv("FOO", ""), "bar")
  proc$stop()
  expect_equal(Sys.getenv("FOO", ""), "foo")
})
