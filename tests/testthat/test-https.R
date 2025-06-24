test_that("HTTPS", {
  skip_on_cran()
  app <- new_app()
  app$get("/hello", function(req, res) {
    res$send("Hello there!")
  })
  http <- local_app_process(app, port = "0s")
  cainfo <- system.file("cert/localhost/ca.crt", package = "webfakes")
  resp <- if (.Platform$OS.type == "windows") {
    callr_curl(http$url("/hello"), list(cainfo = cainfo))
  } else {
    curl::curl_fetch_memory(
      http$url("/hello"),
      handle = curl::new_handle(cainfo = cainfo)
    )
  }
  expect_equal(resp$status_code, 200L)
  expect_equal(tolower(resp$scheme), "https")
})

test_that("HTTP + HTTPS", {
  skip_on_cran()
  app <- new_app()
  app$get("/hello", function(req, res) {
    res$send("Hello there!")
  })
  http <- local_app_process(app, port = c("0", "0s"))
  cainfo <- system.file("cert/localhost/ca.crt", package = "webfakes")

  resp1 <- if (.Platform$OS.type == "windows") {
    callr_curl(http$url("/hello"), list(cainfo = cainfo))
  } else {
    curl::curl_fetch_memory(
      http$url("/hello"),
      handle = curl::new_handle(cainfo = cainfo)
    )
  }
  expect_equal(resp1$status_code, 200L)
  expect_equal(tolower(resp1$scheme), "http")

  resp2 <- if (.Platform$OS.type == "windows") {
    callr_curl(http$url("/hello", https = TRUE), list(cainfo = cainfo))
  } else {
    curl::curl_fetch_memory(
      http$url("/hello", https = TRUE),
      handle = curl::new_handle(cainfo = cainfo)
    )
  }
  expect_equal(resp2$status_code, 200L)
  expect_equal(tolower(resp2$scheme), "https")
})

test_that("Redirect HTTP to HTTPS", {
  skip_on_cran()
  app <- new_app()
  app$get("/hello", function(req, res) {
    res$send("Hello there!")
  })
  http <- local_app_process(app, port = c("0r", "0s"))
  cainfo <- system.file("cert/localhost/ca.crt", package = "webfakes")

  resp1 <- if (.Platform$OS.type == "windows") {
    callr_curl(http$url("/hello"), list(cainfo = cainfo))
  } else {
    curl::curl_fetch_memory(
      http$url("/hello"),
      handle = curl::new_handle(cainfo = cainfo)
    )
  }
  expect_equal(resp1$status_code, 200L)
  expect_equal(tolower(resp1$scheme), "https")

  resp2 <- if (.Platform$OS.type == "windows") {
    callr_curl(http$url("/hello", https = TRUE), list(cainfo = cainfo))
  } else {
    curl::curl_fetch_memory(
      http$url("/hello", https = TRUE),
      handle = curl::new_handle(cainfo = cainfo)
    )
  }
  expect_equal(resp2$status_code, 200L)
  expect_equal(tolower(resp2$scheme), "https")
})
