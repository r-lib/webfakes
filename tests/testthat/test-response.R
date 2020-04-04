
proc <- setup({
  app <- new_app()
  app$locals$applocal <- "foo"

  app$get("/local", function(req, res) {
    res$locals$reslocal <- "bar"
    "next"
  })
  app$get("/local", function(req, res) {
    res$send(paste(res$locals$applocal, res$locals$reslocal))
  })

  new_app_process(app)
})

teardown(proc$stop())

test_that("response locals", {
  url <- proc$get_url("/local")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(rawToChar(resp$content), "foo bar")
})

# on_response is tested via mw_log()
