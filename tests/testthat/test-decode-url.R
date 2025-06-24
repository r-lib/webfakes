test_that("decode_url option", {
  app <- webfakes::new_app()
  app$get(
    webfakes::new_regexp("^/hello/(?<path>.*)$"),
    function(req, res) {
      res$send(paste0("Return content of ", req$params$path, "!"))
    }
  )
  web <- webfakes::local_app_process(app)

  resp <- curl::curl_fetch_memory(web$url("/hello/foo%2fbar/suffix"))
  expect_equal(resp$status_code, 200L)
  expect_equal(
    rawToChar(resp$content),
    "Return content of foo/bar/suffix!"
  )

  app <- webfakes::new_app()
  app$get(
    webfakes::new_regexp("^/hello/(?<path>.*)$"),
    function(req, res) {
      res$send(paste0("Return content of ", req$params$path, "!"))
    }
  )
  web <- webfakes::local_app_process(
    app,
    opts = server_opts(remote = TRUE, decode_url = FALSE)
  )

  resp <- curl::curl_fetch_memory(web$url("/hello/foo%2fbar/suffix"))
  expect_equal(resp$status_code, 200L)
  expect_equal(
    rawToChar(resp$content),
    "Return content of foo%2fbar/suffix!"
  )
})
