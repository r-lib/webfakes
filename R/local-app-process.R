
#' @export

local_app_process <- function(app, ..., .env = parent.frame(),
                              .teardown_env = "auto") {

  cleanup <- TRUE

  if (identical(.teardown_env, "auto")) {
     type <- detect_testthat_type()$type
     if (type == "setup-file") {
       # You must have a teardown- file, nothing to do for us
       cleanup <- FALSE

     } else if (type == "test-file") {
       if (type$edition == 2) {
         .teardown_env <- .env
       } else {
         .teardown_env <- testthat::teardown_env()
       }

     } else if (type == "test-block") {
       .teardown_env <- .env

     } else {
       .teardown_env <- .env
     }
  }

  proc <- do.call("new_app_process", list(app, ...), envir = .env)
  if (cleanup) withr::defer(proc$stop(), envir = .teardown_env)
  proc
}

#' Try to detect where we are, to optimize the process cleanup
#'
#' Scenarios we aim to handle:
#' * In a `setup-*` file, while we are running the tests of a package.
#'   The app must be cleaned up at the end of the test suite.
#' * In a `test-*` file, while we are running the tests of a package.
#'   The app must be cleaned up at the end of the test file.
#' * In a `test_that()` block. Either while running the tests of a
#'   package, or running `test_that()` directly. The app must be
#'   cleaned up at the end of the `test_that()` block.
#' * None of the others, we are running the tests interactively.
#'   The app must be cleaned up manually, or at the end of the R
#'   session.
#'
#' @noRd

detect_testthat_type <- function() {

  # Are we in testthat? This is set in test_files(), and
  # also in test_that(). Rstudio calls test_file(), but that
  # calls test_files(), so that's fine as well.
  is_testthat <- identical(Sys.getenv("TESTTHAT"), "true")

  # Which edition are we running? We _can_ have an edition even if
  # testthat is not running, e.g. if we are running the test cases
  # manually. So `edition_get()` also look at DESCRIPTION. If
  # there is no `DESCRIPTION` it will return 2. For older testthat
  # versions, this function does not exist, so return 2.
  edition <- tryCatch(
    testthat::edition_get(),
    error = function(err) 2L
  )

  # Are we in test_that()? This can only happen if testthat is running
  is_test_block <- is_testthat &&
    is_on_stack(quote(testthat::test_that))

  # Are we in a test file?
  is_test_file <- is_testthat &&
    is_on_stack(quote(testthat::source_file))

  # Are we in a setup file?
  is_setup_file <- is_testthat &&
    is_on_stack(quote(testthat::test_files))

  # Otherwise we are running this manually?
  type <- if (is_setup_file) {
    "setup-file"
  } else if (is_test_file) {
    "test-file"
  } else if (is_test_block) {
    "test-block"
  } else {
    "interactive"
  }

  version <- tryCatch(
    as.character(packageVersion("testthat")),
    error = function(err) NA_character
  )

  list(
    type = type,
    is_testthat = is_testthat,
    edition = edition,
    is_test_block = is_test_block,
    is_test_file = is_test_file,
    is_setup_file = is_setup_file,
    version = version
  )
}

is_on_stack <- function(symbol) {
  for (call in sys.calls()) {
    if (identical(symbol, call[[1]])) return(TRUE)
    if (length(symbol) == 3 &&
        identical(symbol[[1]], `::`) &&
        identical(symbol[[3]], call[[1]])) return(TRUE)
  }

  FALSE
}
