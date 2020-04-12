
web <- setup({
  app <- new_app()
  app$locals$applocal <- "foo"
  app$engine("txt", tmpl_glue())
  app$get("/local", function(req, res) {
    res$locals$reslocal <- "bar"
    "next"
  })
  app$get("/local", function(req, res) {
    res$send(paste(res$locals$applocal, res$locals$reslocal))
  })
  app$get("/badengine", function(req, res) {
    txt <- res$render("foobar")
    res$send(txt)
  })
  app$get("/badjson", function(req, res) {
    res$send_json(1:3, text = "foo")
  })
  app$get("/file", function(req, res) {
    res$send_file(
      root = system.file(package = "presser"),
      file.path("examples", "static", "public", "foo", "bar.json")
    )
  })
  app$get("/type", function(req, res) {
    res$set_type("json")
    res$send("{ \"foo\": 1 }")
  })

  new_app_process(app)
})

teardown(web$stop())

test_that("response locals", {
  url <- web$get_url("/local")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(rawToChar(resp$content), "foo bar")
})

# on_response is tested via mw_log()

test_that("render", {
  # if the template or engine does not exist
  url <- web$get_url("/badengine")
  resp <- curl::curl_fetch_memory(url)
  expect_match(
    rawToChar(resp$content),
    "Cannot find template engine for view"
  )
})

test_that("send_json", {
  url <- web$get_url("/badjson")
  resp <- curl::curl_fetch_memory(url)
  expect_match(
    rawToChar(resp$content),
    "Specify only one of"
  )
})

test_that("send_file", {
  url <- web$get_url("/file")
  resp <- curl::curl_fetch_memory(url)
  path <- system.file(
    package = "presser",
    "examples", "static", "public", "foo", "bar.json"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("set_type", {
  url <- web$get_url("/type")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`content-type`, "application/json")
})
