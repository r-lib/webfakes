
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
  # submit a form
  login_resp <- curl::curl_fetch_memory(tpapp$url("/login"))
  expect_equal(login_resp$status_code, 200L)
  expect_equal(login_resp$type, "text/html")

  html <- rawToChar(login_resp$content)
  xml <- xml2::read_html(html)
  form <- xml2::xml_find_first(xml, "//form")
  input <- xml2::xml_find_first(form, "//input")

  actn <- xml2::xml_attr(form, "action")
  stnm <- xml2::xml_attr(input, "name")
  stvl <- xml2::xml_attr(input, "value")

  data <- charToRaw(paste0(
    stnm, "=", stvl, "&",
    "action=yes"
  ))

  handle2 <- curl::new_handle()
  curl::handle_setheaders(
    handle2,
    "content-type" = "application/x-www-form-urlencoded"
  )
  curl::handle_setopt(
    handle2,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )
  token_resp <- curl::curl_fetch_memory(rsapp$url(actn), handle = handle2)
  expect_equal(token_resp$status_code, 200L)
  token <- jsonlite::fromJSON(rawToChar(token_resp$content))
  expect_match(token$access_token, "^token-[0-9a-f]+$")
})
