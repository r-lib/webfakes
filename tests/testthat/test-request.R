
test_that("parse_query", {
  expect_snapshot({
    parse_query("foo")
    parse_query("?foo")
    parse_query("?foo&bar")
    parse_query("?foo&bar=baz")
  })
})
