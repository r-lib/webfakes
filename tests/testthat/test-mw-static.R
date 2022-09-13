
app <- new_app()
app$use(mw_etag())
app$use(mw_static(root = test_path("fixtures", "static")))
set_headers <- function(req, res) {
  res$set_header("foo", "bar")
}
app$use(mw_static(root = test_path("fixtures", "static2"),
                  set_headers = set_headers))
app$get("/static.html", function(req, res) {
  res$send("this is never reached")
})
app$get("/fallback", function(req, res) {
  res$send("this is the fallback")
})
web <- local_app_process(app)

test_that("static file", {
  url <- web$url("/static.html")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  # type is set from the file extension
  expect_equal(resp$type, "text/html")
  expect_equal(
    rawToChar(resp$content),
    read_char(test_path("fixtures", "static", "static.html"))
  )
})

test_that("static file in subdirectory", {
  url <- web$url("/subdir/static.json")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  # type is set from the file extension
  expect_equal(resp$type, "application/json")
  expect_equal(
    rawToChar(resp$content),
    read_char(test_path("fixtures", "static", "subdir", "static.json"))
  )
})

test_that("file not found", {
  url <- web$url("/notfound")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
})

test_that("file not found falls back", {
  url <- web$url("/fallback")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "text/plain")
  expect_equal(
    rawToChar(resp$content),
    "this is the fallback"
  )
})

test_that("directory is 404", {
  url <- web$url("/subdir")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
})

test_that("set_headers callback", {
  url <- web$url("/static.tar.gz")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "application/gzip")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$foo, "bar")
  expect_equal(
    resp$content,
    read_bin(test_path("fixtures", "static2", "static.tar.gz"))
  )
})

test_that("if-none-match is respected", {
  url <- web$url("/static.html")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  etag <- curl::parse_headers_list(resp$headers)$etag

  h <- curl::new_handle()
  curl::handle_setheaders(h, "If-None-Match" = etag)
  resp2 <- curl::curl_fetch_memory(url, handle = h)
  expect_equal(resp2$status_code, 304L)
  expect_equal(resp2$content, raw(0))
})
