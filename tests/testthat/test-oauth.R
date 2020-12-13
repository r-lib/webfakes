
test_that("oauth2", {
  skip_on_cran()

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

test_that("oauth + httr", {
  skip_on_cran()
  # Not great, the OS should allocate a port, really...
  withr::local_envvar(c(
    HTTP_SERVER = "127.0.0.1",
    HTTP_SERVER_PORT = httpuv::randomPort()
  ))

  # Create the resource server
  rsappex <- oauth2_resource_app(access_duration = 0.1)
  log <- tempfile("webfakes-log-", fileext = ".log")
  on.exit(unlink(log, recursive = TRUE), add = TRUE)
  rsappex$use(logger = mw_log(stream = log), .first = TRUE)
  rsapp <- local_app_process(
    rsappex,
    opts = server_opts(num_threads = 3)
  )
  regi_url <- rsapp$url("/register")
  auth_url <- rsapp$url("/authorize")
  toke_url <- rsapp$url("/token")

  # Register httr
  url <- paste0(
    regi_url,
    "?name=httr%20local%20app",
    "&redirect_uri=", httr::oauth_callback()
  )
  reg_resp <- httr::GET(url)
  httr::stop_for_status(reg_resp)
  regdata <- httr::content(reg_resp)

  app <- httr::oauth_app(
    regdata$name[[1]],
    key = regdata$client_id[[1]],
    secret = regdata$client_secret[[1]],
    redirect_uri = httr::oauth_callback()
  )

  endpoint <- httr::oauth_endpoint(
    authorize = auth_url,
    access = toke_url
  )

  token <- suppressMessages(oauth2_httr_login(
    httr::oauth2.0_token(endpoint, app, cache = FALSE)
  ))

  # This will refresh the token automatically
  Sys.sleep(0.1)
  expect_message(
    cnt <- httr::content(
      httr::GET(rsapp$url("/data"), config = token),
      as = "parsed", type = "application/json"
    ),
    "Auto-refreshing stale OAuth"
  )

  expect_equal(cnt, list(data = list("top secret!")))

  loglines <- readLines(log)

  endpoints <- parse_url(map_chr(strsplit(loglines, " "), "[[", 2))$path
  expect_equal(
    endpoints,
    c("/register", "/authorize", "/authorize/decision",
      "/token", "/data", "/token", "/data")
  )
  status <- map_chr(strsplit(loglines, " "), "[[", 3)
  expect_equal(
    as.numeric(status),
    c(200, 200, 302, 200, 401, 200, 200)
  )
})
