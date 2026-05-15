test_that("invalid handler", {
  app <- new_app()
  expect_snapshot(error = TRUE, {
    app$use("foobar")
    app$get("/foo", 1:100)
  })
})

test_that("handler can declare a locals argument", {
  app <- new_app()
  app$use(function(req, res, locals) {
    if (is.null(locals$num)) locals$num <- 0L
    locals$num <- locals$num + 1L
    "next"
  })
  app$get("/count", function(req, res, locals) {
    res$send(as.character(locals$num))
  })

  web <- local_app_process(app)
  expect_equal(
    rawToChar(curl::curl_fetch_memory(web$url("/count"))$content),
    "1"
  )
  expect_equal(
    rawToChar(curl::curl_fetch_memory(web$url("/count"))$content),
    "2"
  )
})
