
web <- setup({
  app <- new_app()
  app$use(mw_raw())
  app$post("/raw", function(req, res) {
    res$
      set_type("application/octet-stream")$
      send(req$raw)
  })
  new_app_process(app)
})

teardown(web$stop())

test_that("raw body parser", {
  url <- web$get_url("/raw")
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "content-type" = "application/octet-stream")
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(data, resp$content)
})

test_that("non-matching content-type", {
  url <- web$get_url("/raw")
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "content-type" = "application/json")
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 404L)
})
