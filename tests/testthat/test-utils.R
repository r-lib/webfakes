test_that("http_time_stamp ignores local locale", {
  withr::local_locale(LC_TIME = "es_ES.UTF-8")

  Sys.getlocale("LC_TIME") |>
    expect_equal("es_ES.UTF-8")

  # If httr::parse_http_date works it means time is not in local locale
  http_time_stamp() |>
    httr::parse_http_date() |>
    is.na() |>
    expect_false()
})
