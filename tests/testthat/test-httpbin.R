
web <- setup(new_app_process(httpbin_app()))
teardown(web$stop())

# HTTP methods =========================================================

test_that("/get", {
  url <- web$url("/get", query = c(q1 = "one", q2 = "two"))
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
  url <- web$url("/post", query = c(q1 = "one", q2 = "two"))
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

test_that("/post and multipart data", {
  on.exit(rm(tmp), add = TRUE)
  tmp <- tempfile()
  writeBin(charToRaw("foobar\n"), con = tmp)
  url <- web$url("/post")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, customrequest = "POST")
  curl::handle_setform(
    handle, a = "1", b = "2",
    c = curl::form_file(tmp, type = "application/octet-stream")
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(resp$status_code, 200L)
  expect_equal(echo$form, list(a = "1", b = "2"))
  expect_equal(
    echo$files$c,
    list(
      filename = basename(tmp),
      value = paste0(
        "data:application/octet-stream;base64,",
        base64_encode("foobar\n")
      )
    )
  )
})

# Auth =================================================================

test_that("/basic-auth", {
  # no auth supplied
  url <- web$url("/basic-auth/Aladdin/OpenSesame")
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

test_that("/bearer", {
  # no auth
  url <- web$url("/bearer")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 401L)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`www-authenticate`, "bearer")

  # bad auth format
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "authorization", "foobar")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 401L)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`www-authenticate`, "bearer")

  # correct format
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, authorization = "Bearer secret")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo, list(authenticated = TRUE, token = "secret"))
})

# Status codes =========================================================

test_that("/status", {
  codes <- c(200, 301, 401, 404)

  # get
  for (code in codes) {
    url <- web$url(paste0("/status/", code))
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
    url <- web$url(paste0("/status/", code))
    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status_code, code)
  }
})

# Request inspection ===================================================

test_that("/headers", {
  url <- web$url("/headers")
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "header1" = "this", "header2" = "that")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo$headers$header1, "this")
  expect_equal(echo$headers$header2, "that")
})

test_that("/ip", {
  url <- web$url("/ip")
  resp <- curl::curl_fetch_memory(url)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo, list(origin = "127.0.0.1"))
})

test_that("/user-agent", {
  url <- web$url("/user-agent")
  ua <- "i am libcurl, that is my name"
  withr::local_options(list(HTTPUserAgent = ua))
  resp <- curl::curl_fetch_memory(url)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo, list("user-agent" = ua))
})

# Response inspection ==================================================

test_that("/etag", {
  url <- web$url("/etag/foobar")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 200)
  expect_equal(headers$etag, "foobar")

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-None-Match" = "\"foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 304)
  expect_true(length(resp$content) == 0)

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-None-Match" = "\"not-foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 200)
  expect_true(length(resp$content) > 0)

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-Match" = "\"foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 200)
  expect_true(length(resp$content) > 0)

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-Match" = "\"not-foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 412)
})

# Response formats =====================================================

test_that("/deny", {
  url <- web$url("/deny")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  path <- system.file(
    package = "presser",
    "examples", "httpbin", "data", "deny.txt"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("/gzip", {
  # curl seems to ungzip automatically, so we use url()
  url <- web$url("/gzip")
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  echo <- readBin(con, "raw", 10000)
  expect_equal(echo[1:2], charToRaw("\x1f\x8b"))
  json <- readChar(gzcon(rawConnection(echo)), 10000, useBytes = TRUE)
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(obj$path, "/gzip")
})

test_that("/encoding/utf8", {
  url <- web$url("/encoding/utf8")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "text/html; charset=utf-8")
  ptrn <- as.raw(c(
    0xe1, 0x8c, 0x8c, 0xe1, 0x8c, 0xa5, 0x20, 0xe1, 0x8b,
    0xab, 0xe1, 0x88, 0x88, 0xe1, 0x89, 0xa4, 0xe1, 0x89, 0xb1
  ))
  # On windows end of line is converted to \r\n
  expect_true(grepRaw(ptrn, resp$content, fixed = TRUE) %in% c(9085, 9233))
})

test_that("/html", {
  url <- web$url("/html")
  resp <- curl::curl_fetch_memory(url)
  expect_match(resp$type, "^text/html")
  expect_match(rawToChar(resp$content), "<html>", fixed = TRUE)
})

test_that("/json", {
  url <- web$url("/json")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/json")
  path <- system.file(
    package = "presser",
    "examples", "httpbin", "data", "example.json"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("/robots.txt", {
  url <- web$url("/robots.txt")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "text/plain")
  path <- system.file(
    package = "presser",
    "examples", "httpbin", "data", "robots.txt"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("/xml", {
  url <- web$url("/xml")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/xml")
  path <- system.file(
    package = "presser",
    "examples", "httpbin", "data", "example.xml"
  )
  expect_equal(resp$content, read_bin(path))
})

# Dynamic data =========================================================

test_that("/base64/", {
  url <- web$url("/base64")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  expect_equal(rawToChar(resp$content), "Everything is Rsome")

  value <- base64_encode("hello there!")
  url <- web$url(paste0("/base64/", value))
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/octet-stream")
  expect_equal(rawToChar(resp$content), "hello there!")
})

test_that("/bytes", {
  url <- web$url("/bytes/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)

  url <- web$url("/bytes/1000")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/octet-stream")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers[["content-length"]], "1000")
  expect_equal(length(resp$content), 1000)
})

test_that("/delay", {
  url <- web$url("/delay/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)

  url <- web$url("/delay/0.1")
  st <- system.time(resp <- curl::curl_fetch_memory(url))
  expect_true(st[["elapsed"]] >= 0.1)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "application/json")
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo$path, "/delay/0.1")
})

test_that("/uuid", {
  url <- web$url("/uuid")
  resp <- curl::curl_fetch_memory(url)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_match(echo$uuid, "^[-0-9a-z]+$")
  expect_equal(nchar(echo$uuid), 36)
})

# Cookies ==============================================================

# Images ===============================================================

test_that("/image", {
  url <- web$url("/image")
  types <- c("image/webp", "image/svg+xml", "image/jpeg", "image/png")
  for (type in types) {
    handle <- curl::new_handle()
    curl::handle_setheaders(handle, accept = type)
    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status_code, 200L)
    expect_equal(resp$type, type)
  }

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, accept = "image/*")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "image/png")

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, accept = "application/json")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 406L)

  exts <- c("jpeg", "png", "svg", "webp")
  for (ext in exts) {
    handle <- curl::new_handle()
    url <- web$url(paste0("/image/", ext))
    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status_code, 200L)
    expect_equal(resp$type, unname(mime_find(ext)))
  }
})

# Redirects ============================================================

test_that("absolute-redirect", {
  url <- web$url("/absolute-redirect/4")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 302L)
  expect_equal(headers$location, web$url("/absolute-redirect/3"))

  url <- web$url("/absolute-redirect/2")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = TRUE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$url, web$url("/get"))

  url <- web$url("/absolute-redirect/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
})

test_that("redirect", {
  url <- web$url("/redirect/4")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 302L)
  expect_equal(headers$location, "/redirect/3")

  url <- web$url("/redirect/2")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = TRUE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$url, web$url("/get"))

  url <- web$url("/redirect/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
})

test_that("/redirect-to", {
  # default status is 302
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  url <- web$url("/redirect-to", query = list(url = "/get"))
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 302L)
  expect_equal(resp$type, "text/plain")
  expect_equal(curl::parse_headers_list(resp$headers)$location, "/get")
  expect_equal(rawToChar(resp$content), "302 Found. Redirecting to /get")

  # can specify status
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  url <- web$url(
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
