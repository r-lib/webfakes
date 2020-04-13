
web <- setup({
  app <- new_app()
  app$engine("html", tmpl_glue())
  app$set_config("views", test_path("fixtures", "views"))
  app$get("/hello/:user", function(req, res) {
    locals <- c(as.list(req$params), greeting = "hello")
    html <- res$render("test-view", locals = locals)
    res$
      set_type("text/html")$
      send(html)
  })
  new_app_process(app)
})

teardown(web$stop())

test_that("glue templating", {
  url <- web$url("/hello/gabor")
  resp <- curl::curl_fetch_memory(url)
  expect_match(rawToChar(resp$content), "hello gabor")
})
