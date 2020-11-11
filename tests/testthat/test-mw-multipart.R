
app <- new_app()
app$use(mw_multipart())
app$put("/form", function(req, res) {
  ret = list(form = req$form, files = req$files)
  res$send_json(ret, pretty = TRUE, auto_unbox = TRUE)
})
web <- local_app_process(app)

test_that("mw_multipart", {
  on.exit(rm(tmp), add = TRUE)
  tmp <- tempfile()
  writeBin(charToRaw("foobar\n"), con = tmp)
  url <- web$url("/form")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, customrequest = "PUT")
  curl::handle_setform(
    handle, a = "1", b = "2",
    c = curl::form_file(tmp, type = "text/plain")
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo$form, list(a = "1", b = "2"))
  expect_equal(
    echo$files$c,
    list(
      filename = basename(tmp),
      value = base64_encode("foobar\n")
    )
  )
})
