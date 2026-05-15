test_that("http_time_stamp ignores local locale", {
  skip_on_cran()
  envir <- environment()
  tryCatch(
    withr::local_locale(LC_TIME = "es_ES.UTF-8", .local_envir = envir),
    warning = function(w) skip("es_ES.UTF-8 locale not available"),
    error = function(e) skip("es_ES.UTF-8 locale not available")
  )

  expect_equal(Sys.getlocale("LC_TIME"), "es_ES.UTF-8")

  # If httr::parse_http_date works it means time is not in local locale
  expect_false(is.na(httr::parse_http_date(http_time_stamp())))
})
