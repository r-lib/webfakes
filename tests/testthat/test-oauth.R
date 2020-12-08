
test_that("oauth2_resource_app", {

  # Create the resource server
  rsapp <- local_app_process(
    oauth2_resource_app(),
    opts = server_opts(num_threads = 3)
  )
  regi_url <- rsapp$url("/register")
  auth_url <- rsapp$url("/authorize")
  toke_url <- rsapp$url("/token")

  # Create the third party app server
  tpapp <- local_app_process(
    oauth2_third_party_app("3P app"),
    opts = server_opts(num_threads = 3)
  )
  redi_url <- tpapp$url("/login/redirect")
  conf_url <- tpapp$url("/login/config")

  # Register the third party app at the resource server
  # In real life this is done by the admin of the third party app
  url <- paste0(
    regi_url,
    "?name=3P%20app",
    "&redirect_uri=", redi_url
  )
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  regdata <- jsonlite::fromJSON(rawToChar(resp$content))

  # Now set this data on the third party app
  # In real life this is included in the config of the third party app
  # by its admin
  auth_data <- jsonlite::toJSON(list(
    auth_url = auth_url,
    token_url = toke_url,
    client_id = regdata$client_id,
    client_secret = regdata$client_secret
  ), auto_unbox = TRUE)
  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "content-type" = "application/json"
  )
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = nchar(auth_data),
    postfields = auth_data
  )
  resp2 <- curl::curl_fetch_memory(conf_url, handle = handle)
  expect_equal(resp2$status_code, 200L)

  # Now everything is set up, a user can go to the login page of the
  # third party app:
  # browseURL(tpapp$url("/login"))

  # Scripting it is a bit tedious, because we need to parse the HTML and
  # submit a form. The `oauth2_login()` helper function does this.
  resp3 <- oauth2_login(tpapp$url("/login"))
  token <- jsonlite::fromJSON(rawToChar(resp3$token_response$content))

  expect_equal(resp3$login_response$status_code, 200L)
  expect_equal(resp3$login_response$type, "text/html")
  expect_equal(resp3$token_response$status_code, 200L)
  expect_match(token$access_token, "^token-[0-9a-f]+$")
})
