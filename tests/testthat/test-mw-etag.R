app <- new_app()$use(mw_etag())$get("/txt", function(req, res) {
  res$set_type("text/plain")$send("textual")
})$get("/txt-empty", function(req, res) {
  res$set_type("text/plain")$send("")
})$get("/raw", function(req, res) {
  res$set_type("applicartion/octet-stream")$send(charToRaw("textual"))
})$get("/raw-empty", function(req, res) {
  res$set_type("applicartion/octet-stream")$send(raw(0))
})

web <- local_app_process(app)

test_that("text/plain response", {
  url <- web$url("/txt")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_true("etag" %in% names(headers))
  expect_match(headers$etag, "\"[-a-zA-Z0-9]+\"")
})

test_that("raw response", {
  txt <- curl::curl_fetch_memory(web$url("/txt"))
  raw <- curl::curl_fetch_memory(web$url("/raw"))
  htxt <- curl::parse_headers_list(txt$headers)
  hraw <- curl::parse_headers_list(raw$headers)
  expect_equal(htxt$etag, hraw$etag)
})

test_that("etag for empty response", {
  txt <- curl::curl_fetch_memory(web$url("/txt-empty"))
  raw <- curl::curl_fetch_memory(web$url("/raw-empty"))
  htxt <- curl::parse_headers_list(txt$headers)
  hraw <- curl::parse_headers_list(raw$headers)
  expect_equal(htxt$etag, "\"00000000\"")
  expect_equal(htxt$etag, hraw$etag)
})
