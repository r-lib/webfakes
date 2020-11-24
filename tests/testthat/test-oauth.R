
test_that("oauth2_resource_app", {
  web <- local_app_process(oauth2_resource_app(redirect_uri = "https://dummy"))
  auth <- web$url(
    "/authorize",
    query = c(
      client_id = "client_id",
      redirect_uri = "https://dummy",
      state = "mystate"
    )
  )

  resp <- curl::curl_fetch_memory(auth)

  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "text/html")
  html <- rawToChar(resp$content)
  expect_match(html, "https://dummy\\?code=[a-f0-9]+&state=mystate")

  # Get the code, and request a token with it
  code <- sub("^.*code=([a-f0-9]+).*$", "\\1", html)

  token <- web$url("/token")
  handle <- curl::new_handle()
  data <- charToRaw(paste0(
    "grant_type=authorization_code&",
    "code=", code, "&",
    "client_id=client_id&",
    "client_secret=client_secret&",
    "redirect_uri=https://dummy"
  ))
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
  resp2 <- curl::curl_fetch_memory(token, handle = handle)

  expect_equal(resp2$status_code, 200L)
  token <- jsonlite::fromJSON(rawToChar(resp2$content), simplifyVector = FALSE)
  expect_match(token$access_token, "^token-[a-f0-9]+$")
})
