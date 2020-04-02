
proc <- setup(new_app_process(httpbin_app()))
teardown(proc$stop())

# HTTP methods =========================================================

test_that("/get", {
  url <- proc$get_url("/get", query = c(q1 = "one", q2 = "two"))
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "foo" = "bar")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "application/json")
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(data$path, "/get")
  expect_equal(data$headers$foo, "bar")
  expect_equal(data$args, list(q1 = "one", q2 = "two"))
})

test_that("/post", {
  url <- proc$get_url("/post", query = c(q1 = "one", q2 = "two"))
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "content-type" = "application/json",
    foo = "bar"
  )
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "application/json")
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(data$path, "/post")
  expect_equal(data$headers$foo, "bar")
  expect_equal(data$args, list(q1 = "one", q2 = "two"))
  expect_equal(
    data$json,
    list(foo = "bar", foobar = 1:3)
  )
  expect_equal(
    jsonlite::fromJSON(data$data, simplifyVector = TRUE),
    data$json
  )
})

# Auth =================================================================

test_that("/basic-auth", {
  # no auth supplied
  url <- proc$get_url("/basic-auth/Aladdin/OpenSesame")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 401L)
  expect_equal(resp$type, "text/plain")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`www-authenticate`, "Basic realm=\"Fake Realm\"")

  # correct auth
  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "Authorization"= "Basic QWxhZGRpbjpPcGVuU2VzYW1l"
  )
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "application/json")
  expect_equal(
    jsonlite::fromJSON(rawToChar(resp$content)),
    list(authenticated = TRUE, user = "Aladdin")
  )

  # wrong auth
  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "Authorization"= "Basic NOLUCK"
  )
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 401L)
  expect_equal(resp$type, "text/plain")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`www-authenticate`, "Basic realm=\"Fake Realm\"")
})

# Status codes =========================================================

test_that("/status", {
  codes <- c(200, 301, 401, 404)

  # get
  for (code in codes) {
    url <- proc$get_url(paste0("/status/", code))
    resp <- curl::curl_fetch_memory(url)
    expect_equal(resp$status_code, code)
  }

  # post
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "content-type" = "application/json")
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )
  for (code in codes) {
    url <- proc$get_url(paste0("/status/", code))
    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status_code, code)
  }
})

# Request inspection ===================================================

# Response inspection ==================================================

# Response formats =====================================================

# Dynamic data =========================================================

# Cookies ==============================================================

# Images ===============================================================

# Redirects ============================================================

test_that("/redirect-to", {
  # default status is 302
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  url <- proc$get_url("/redirect-to", query = list(url = "/get"))
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 302L)
  expect_equal(resp$type, "text/plain")
  expect_equal(curl::parse_headers_list(resp$headers)$location, "/get")
  expect_equal(rawToChar(resp$content), "302 Found. Redirecting to /get")

  # can specify status
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  url <- proc$get_url(
    "/redirect-to",
    query = list(url = "/status/200", status_code = 301)
  )
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 301L)
  expect_equal(resp$type, "text/plain")
  expect_equal(curl::parse_headers_list(resp$headers)$location, "/status/200")
  expect_equal(
    rawToChar(resp$content),
    "301 Moved Permanently. Redirecting to /status/200"
  )
})

# Anything =============================================================
