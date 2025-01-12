
test_that("HTTPS", {
  app <- new_app()
  app$get("/hello", function(req, res) {
    res$send("Hello there!")
  })
  http <- local_app_process(app, port = "0s")
  cainfo <- system.file("cert/localhost/ca.crt", package = "webfakes")
  resp <- curl::curl_fetch_memory(
    http$url("/hello"),
    handle = curl::new_handle(cainfo = cainfo)
  )
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$scheme, "HTTPS")
})

test_that("HTTP + HTTPS", {
  app <- new_app()
  app$get("/hello", function(req, res) {
    res$send("Hello there!")
  })
  http <- local_app_process(app, port = c("0", "0s"))
  cainfo <- system.file("cert/localhost/ca.crt", package = "webfakes")

  resp1 <- curl::curl_fetch_memory(
    http$url("/hello"),
    handle = curl::new_handle(cainfo = cainfo)
  )
  expect_equal(resp1$status_code, 200L)
  expect_equal(resp1$scheme, "HTTP")

  resp2 <- curl::curl_fetch_memory(
    http$url("/hello", https = TRUE),
    handle = curl::new_handle(cainfo = cainfo)
  )
  expect_equal(resp2$status_code, 200L)
  expect_equal(resp2$scheme, "HTTPS")
})

test_that("Redirect HTTP to HTTPS", {
  app <- new_app()
  app$get("/hello", function(req, res) {
    res$send("Hello there!")
  })
  http <- local_app_process(app, port = c("0r", "0s"))
  cainfo <- system.file("cert/localhost/ca.crt", package = "webfakes")

  resp1 <- curl::curl_fetch_memory(
    http$url("/hello"),
    handle = curl::new_handle(cainfo = cainfo)
  )
  expect_equal(resp1$status_code, 200L)
  expect_equal(resp1$scheme, "HTTPS")

  resp2 <- curl::curl_fetch_memory(
    http$url("/hello", https = TRUE),
    handle = curl::new_handle(cainfo = cainfo)
  )
  expect_equal(resp2$status_code, 200L)
  expect_equal(resp2$scheme, "HTTPS")
})
