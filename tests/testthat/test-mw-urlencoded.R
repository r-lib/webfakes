
web <- setup({
  app <- new_app()
  app$use(mw_urlencoded())
  app$post("/form", function(req, res) {
    ret <- list(
      form = req$form
    )
    res$send_json(ret, pretty = TRUE, auto_unbox = TRUE)
  })
  new_app_process(app)
})

teardown(web$stop())

test_that("mw-urlencoded", {
  url <- web$url("/form")
  handle <- curl::new_handle()
  data <- charToRaw("foo=bar&foobar=100")
  curl::handle_setheaders(
    handle,
    "content-type" = "application/x-www-form-urlencoded"
  )
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )
  resp <- curl::curl_fetch_memory(url, handle = handle)
  echo <- jsonlite::fromJSON(
    rawToChar(resp$content),
    simplifyVector = FALSE
  )
  expect_equal(echo, list(form = list("foo" = "bar", "foobar" = "100")))
})
