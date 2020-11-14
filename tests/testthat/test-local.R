
test_that("app from setup", {
  # This app was created in setup.R
  url <- httpbin2$url("/get", query = c(q1 = "one", q2 = "two"))
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "foo" = "bar")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
})
