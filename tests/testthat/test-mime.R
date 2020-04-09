
test_that("mime_find", {
  expect_equal(mime_find("json"), c(json = "application/json"))
  expect_equal(mime_find("blahxml"), c(xml = "text/xml"))
})
