
test_that("middleware", {
  expect_true(path_match("foobar", "foobar2", list(method = "use")))
})

test_that("string", {
  good <- list(
    list("/foo", "/foo"),
    list("/", "/"),
    list("/***", "/***")
  )
  for (x in good) {
    expect_true(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }

  bad <- list(
    list("/foo", "/bar"),
    list("/foo", "foo")
  )
  for (x in bad) {
    expect_false(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }
})

test_that("character vector or list", {
  good <- list(
    list("/foo", c("/foo2", "/foo")),
    list("/", list("notthis", "/"))
  )
  for (x in good) {
    expect_true(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }

  bad <- list(
    list("/foo", list())
  )
  for (x in bad) {
    expect_false(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }
})

test_that("regexp", {
  good <- list(
    list("/foo", new_regexp("^/fo+$"), list(params = list())),
    list("/", list(new_regexp("/foobar"), new_regexp("^/$")), list(params = list()))
  )
  for (x in good) {
    expect_equal(
      path_match("get", x[[1]], list(method = "get", path = x[[2]])),
      x[[3]]
    )
  }

  bad <- list(
    list("/foo", list("/foobar"))
  )
  for (x in bad) {
    expect_false(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }
})

test_that("list of things", {
  good <- list(
    list("/foo", list("/foo2", new_regexp("/foo")), list(params = list())),
    list("/", list(new_regexp("notthis"), "/"), TRUE)
  )
  for (x in good) {
    expect_equal(
      path_match("get", x[[1]], list(method = "get", path = x[[2]])),
      x[[3]]
    )
  }

  bad <- list(
    list("/foo", list())
  )
  for (x in bad) {
    expect_false(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }

})

test_that("regexp with capture groups", {
  good <- list(
    list(
      "/foo/bar",
      new_regexp("^/foo/([a-z]+)/?$"),
      list(params = structure(list("bar"), names = ""))
    ),
    list(
      "/foo/bar",
      new_regexp("^/(?<x>f.*)/(?<y>[a-z]+)/?$"),
      list(params = structure(list("foo", "bar"), names = c("x", "y")))
    )
  )
  for (x in good) {
    expect_equal(
      path_match("get", x[[1]], list(method = "get", path = x[[2]])),
      x[[3]]
    )
  }

  bad <- list(
    list("/foo/bar", new_regexp("/foox/([a-z]+)/?$"))
  )
  for (x in bad) {
    expect_false(path_match("get", x[[1]], list(method = "get", path = x[[2]])))
  }
})

test_that("tokens", {
  good <- list(
    list(
      "/foo",
      "/:x",
      list(params = list(x = "foo"))
    ),
    list(
      "/foo/bar",
      "/:x/:y",
      list(params = list(x = "foo", y = "bar"))
    )
  )
  for (x in good) {
    expect_equal(
      path_match("get", x[[1]], list(method = "get", path = x[[2]])),
      x[[3]]
    )
  }
})
