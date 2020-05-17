
web <- setup(new_app_process(test_response_app()))
teardown(web$stop())

test_that("response locals", {
  url <- web$url("/local")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(rawToChar(resp$content), "foo bar")
})

# on_response is tested via mw_log()

test_that("render", {
  # if the template or engine does not exist
  url <- web$url("/badengine")
  resp <- curl::curl_fetch_memory(url)
  expect_match(
    rawToChar(resp$content),
    "Cannot find template engine for view"
  )
})

test_that("send_json", {
  url <- web$url("/badjson")
  resp <- curl::curl_fetch_memory(url)
  expect_match(
    rawToChar(resp$content),
    "Specify only one of"
  )
})

test_that("send_file", {
  url <- web$url("/file")
  resp <- curl::curl_fetch_memory(url)
  path <- system.file(
    package = "presser",
    "examples", "static", "public", "foo", "bar.json"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("set_type", {
  url <- web$url("/type")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`content-type`, "application/json")
})

test_that("write", {
  url <- web$url("/write")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200)
  expect_equal(rawToChar(resp$content), "hello world!")

  # header can be set
  url <- web$url("/write-header")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200)
  expect_equal(rawToChar(resp$content), "hello world!")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`foo`, "bar")
})

test_that("write-wait", {
  url <- web$url("/write-wait")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200)
  expect_equal(rawToChar(resp$content), "hello world!")
})

test_that("send_chunk", {
  url <- web$url("/send-chunk")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200)
  expect_equal(
    rawToChar(resp$content),
    "first chunk\nsecond chunk\nthird and final chunk\n"
  )
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers[["content-type"]], "text/plain")
  expect_equal(headers[["transfer-encoding"]], "chunked")
})
