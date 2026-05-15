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

named_list <- function() structure(list(), names = character())

test_that("regexp", {
  good <- list(
    list("/foo", new_regexp("^/fo+$"), list(params = named_list())),
    list(
      "/",
      list(new_regexp("/foobar"), new_regexp("^/$")),
      list(params = named_list())
    )
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
    list(
      "/foo",
      list("/foo2", new_regexp("/foo")),
      list(params = named_list())
    ),
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

test_that("trailing slash is ignored for literal and parameterized paths (#120)", {
  cases <- list(
    list("/foo", "/foo/"),
    list("/foo/", "/foo"),
    list("/foo/", "/foo/"),
    list("/foo/bar", "/foo/bar/"),
    list("/foo/bar/", "/foo/bar")
  )
  for (x in cases) {
    expect_true(
      path_match("get", x[[1]], list(method = "get", path = x[[2]])),
      info = paste(x[[1]], "vs", x[[2]])
    )
  }

  param_cases <- list(
    list("/foo", "/:x/", list(params = list(x = "foo"))),
    list("/foo/", "/:x", list(params = list(x = "foo"))),
    list("/foo/", "/:x/", list(params = list(x = "foo")))
  )
  for (x in param_cases) {
    expect_equal(
      path_match("get", x[[1]], list(method = "get", path = x[[2]])),
      x[[3]]
    )
  }

  # Root path is preserved, not stripped to empty
  expect_true(path_match("get", "/", list(method = "get", path = "/")))

  # User-supplied regex is left alone — they're in control
  expect_false(
    isTRUE(path_match(
      "get",
      "/foo/",
      list(method = "get", path = new_regexp("^/foo$"))
    ))
  )
})
