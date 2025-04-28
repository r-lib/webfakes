test_that("error if cannot start", {
  # does not start before the timeout
  app <- new_app()
  app$listen <- function(...) Sys.sleep(1)
  expect_snapshot(
    error = TRUE,
    new_app_process(app, process_timeout = 100, start = TRUE)
  )

  # errors before/while starting
  app <- new_app()
  app$listen <- function(...) stop("oops")
  expect_snapshot(
    error = TRUE,
    new_app_process(app, start = TRUE)
  )

  # sends a different message first
  app <- new_app()
  app$listen <- function(...) "foobar"
  expect_snapshot(
    error = TRUE,
    new_app_process(app, start = TRUE)
  )
})

test_that("get_state", {
  app <- new_app()
  on.exit(proc$stop(), add = TRUE)
  proc <- new_app_process(app, start = TRUE)
  expect_equal(proc$get_state(), "live")
  proc$.process$kill()
  expect_equal(proc$get_state(), "dead")
  expect_output(proc$stop(), "webfakes process dead")
  expect_equal(proc$get_state(), "not running")
})

test_that("env vars", {
  app <- new_app()
  on.exit(proc$stop(), add = TRUE)
  withr::local_envvar(list(FOO = "foo"))
  withr::local_envvar(list(BAR = NA_character_))
  proc <- new_app_process(app, start = TRUE)
  proc$local_env(list(FOO = "bar"))
  proc$local_env(list(BAR = "{url}"))
  expect_equal(Sys.getenv("FOO", ""), "bar")
  expect_equal(Sys.getenv("BAR"), proc$url())
  proc$stop()
  expect_equal(Sys.getenv("FOO", ""), "foo")
  expect_equal(Sys.getenv("BAR", ""), "")
})

test_that("env vars 2", {
  app <- new_app()
  on.exit(proc$stop(), add = TRUE)
  withr::local_envvar(list(FOO = "foo"))
  withr::local_envvar(list(BAR = NA_character_))
  proc <- new_app_process(app)
  proc$local_env(list(FOO = "bar"))
  proc$local_env(list(BAR = "{url}"))
  proc$start()
  expect_equal(Sys.getenv("FOO", ""), "bar")
  expect_equal(Sys.getenv("BAR"), proc$url())
  proc$stop()
  expect_equal(Sys.getenv("FOO", ""), "foo")
  expect_equal(Sys.getenv("BAR", ""), "")
})
