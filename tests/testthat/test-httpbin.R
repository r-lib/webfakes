
# HTTP methods =========================================================

test_that("/get", {
  url <- httpbin$url("/get", query = c(q1 = "one", q2 = "two"))
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
  url <- httpbin$url("/post", query = c(q1 = "one", q2 = "two"))
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
  url <- httpbin$url("/post")
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
  url <- httpbin$url("/basic-auth/Aladdin/OpenSesame")
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

test_that("/hidden-basic-auth", {
  # no auth supplied
  url <- httpbin$url("/hidden-basic-auth/Aladdin/OpenSesame")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
  expect_equal(resp$type, "text/plain")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`www-authenticate`, NULL)

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
  expect_equal(resp$status_code, 404L)
  expect_equal(resp$type, "text/plain")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers$`www-authenticate`, NULL)
})

test_that("/bearer", {
  # no auth
  url <- httpbin$url("/bearer")
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
    url <- httpbin$url(paste0("/status/", code))
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
    url <- httpbin$url(paste0("/status/", code))
    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status_code, code)
  }
})

# Request inspection ===================================================

test_that("/headers", {
  url <- httpbin$url("/headers")
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "header1" = "this", "header2" = "that")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo$headers$header1, "this")
  expect_equal(echo$headers$header2, "that")
})

test_that("/ip", {
  url <- httpbin$url("/ip")
  resp <- curl::curl_fetch_memory(url)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo, list(origin = "127.0.0.1"))
})

test_that("/user-agent", {
  url <- httpbin$url("/user-agent")
  ua <- "i am libcurl, that is my name"
  withr::local_options(list(HTTPUserAgent = ua))
  resp <- curl::curl_fetch_memory(url)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo, list("user-agent" = ua))
})

# Response inspection ==================================================

test_that("/etag", {
  url <- httpbin$url("/etag/foobar")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 200)
  expect_equal(headers$etag, "foobar")

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-None-Match" = "\"foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 304)
  expect_true(length(resp$content) == 0)

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-None-Match" = "\"not-foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200)
  expect_true(length(resp$content) > 0)

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-Match" = "\"foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200)
  expect_true(length(resp$content) > 0)

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "If-Match" = "\"not-foobar\"")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 412)
})

test_that("/response-headers", {
  url <- httpbin$url("/response-headers")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(rawToChar(resp$content), "{}")

  url2 <- httpbin$url("/response-headers", c(foo = "bar"))
  resp2 <- curl::curl_fetch_memory(url2)
  expect_equal(
    jsonlite::fromJSON(rawToChar(resp2$content)),
    list(foo = "bar")
  )
  headers <- curl::parse_headers_list(resp2$headers)
  expect_equal(headers[["foo"]], "bar")

  url3 <- httpbin$url("/response-headers", c(foo = "bar", foobar = "baz"))
  resp3 <- curl::curl_fetch_memory(url3)
  expect_equal(
    jsonlite::fromJSON(rawToChar(resp3$content)),
    list(foo = "bar", foobar = "baz")
  )
  headers <- curl::parse_headers_list(resp3$headers)
  expect_equal(headers[["foo"]], "bar")
  expect_equal(headers[["foobar"]], "baz")

  handle <- curl::new_handle()
  data <- charToRaw("{}")
  curl::handle_setheaders(
    handle,
    "content-type" = "application/json"
  )
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )
  resp31 <- curl::curl_fetch_memory(url3, handle = handle)
  expect_equal(
    jsonlite::fromJSON(rawToChar(resp31$content)),
    list(foo = "bar", foobar = "baz")
  )
  headers <- curl::parse_headers_list(resp3$headers)
  expect_equal(headers[["foo"]], "bar")
  expect_equal(headers[["foobar"]], "baz")

  url4 <- httpbin$url("/response-headers", c(foo = "bar", foo = "bar2"))
  resp4 <- curl::curl_fetch_memory(url4)
  expect_equal(
    jsonlite::fromJSON(rawToChar(resp4$content)),
    list(foo = c("bar", "bar2"))
  )
  headers <- curl::parse_headers_list(resp4$headers)
  expect_equal(
    headers[names(headers) == "foo"],
    list(foo = "bar", foo = "bar2")
  )
})

test_that("/cache", {
  url <- httpbin$url("/cache")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_true("last-modified" %in% tolower(names(headers)))

  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "If-Modified-Since" =
      http_time_stamp(Sys.time() - as.difftime(5, units = "mins"))
  )
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 304L)


  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "If-None-Match" = "some-etag"
  )
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 304L)
})

test_that("/cache/:value", {
  url <- httpbin$url("/cache/10")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(
    headers[["cache-control"]],
    "public, max-age=10"
  )
})

# Response formats =====================================================

test_that("/deny", {
  url <- httpbin$url("/deny")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  path <- system.file(
    package = "webfakes",
    "examples", "httpbin", "data", "deny.txt"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("/gzip", {
  # curl seems to ungzip automatically, so we use url()
  url <- httpbin$url("/gzip")
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  echo <- readBin(con, "raw", 10000)
  expect_equal(echo[1:2], charToRaw("\x1f\x8b"))
  json <- readChar(gzcon(rawConnection(echo)), 10000, useBytes = TRUE)
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(obj$path, "/gzip")
})

test_that("/deflate", {
  url <- httpbin$url("/deflate")
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  echo <- readBin(con, "raw", 10000)
  data <- jsonlite::fromJSON(rawToChar(zip::inflate(echo)$output))
  expect_true(data$deflated)
})

test_that("/brotli", {
  url <- httpbin$url("/brotli")
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  echo <- readBin(con, "raw", 10000)
  data <- jsonlite::fromJSON(rawToChar(brotli::brotli_decompress(echo)))
  expect_true(data$brotli)
})

test_that("/encoding/utf8", {
  url <- httpbin$url("/encoding/utf8")
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
  url <- httpbin$url("/html")
  resp <- curl::curl_fetch_memory(url)
  expect_match(resp$type, "^text/html")
  expect_match(rawToChar(resp$content), "<html>", fixed = TRUE)
})

test_that("/json", {
  url <- httpbin$url("/json")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/json")
  path <- system.file(
    package = "webfakes",
    "examples", "httpbin", "data", "example.json"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("/robots.txt", {
  url <- httpbin$url("/robots.txt")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "text/plain")
  path <- system.file(
    package = "webfakes",
    "examples", "httpbin", "data", "robots.txt"
  )
  expect_equal(resp$content, read_bin(path))
})

test_that("/xml", {
  url <- httpbin$url("/xml")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/xml")
  path <- system.file(
    package = "webfakes",
    "examples", "httpbin", "data", "example.xml"
  )
  expect_equal(resp$content, read_bin(path))
})

# Dynamic data =========================================================

test_that("/base64/", {
  url <- httpbin$url("/base64")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  expect_equal(rawToChar(resp$content), "Everything is Rsome")

  value <- base64_encode("hello there!")
  url <- httpbin$url(paste0("/base64/", value))
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/octet-stream")
  expect_equal(rawToChar(resp$content), "hello there!")
})

test_that("/bytes", {
  url <- httpbin$url("/bytes/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)

  url <- httpbin$url("/bytes/1000")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$type, "application/octet-stream")
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(headers[["content-length"]], "1000")
  expect_equal(length(resp$content), 1000)
})

test_that("/delay", {
  url <- httpbin$url("/delay/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)

  url <- httpbin$url("/delay/0.2")
  st <- system.time(resp <- curl::curl_fetch_memory(url))
  expect_true(st[["elapsed"]] >= 0.1)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$type, "application/json")
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(echo$path, "/delay/0.2")
})

test_that("/stream-bytes", {
  url <- httpbin$url("/stream-bytes/100")
  resp <- curl::curl_fetch_memory(url)
  headers <- curl:::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 200)
  expect_equal(headers[["transfer-encoding"]], "chunked")
  expect_equal(length(resp$content), 100)

  ## seed works
  url2 <- httpbin$url("/stream-bytes/10", c(seed = 100))
  resp2 <- curl::curl_fetch_memory(url2)
  expect_false(identical(resp$content, resp2$content))

  resp3 <- curl::curl_fetch_memory(url2)
  expect_identical(resp2$content, resp3$content)

  ## chunk_size works
  url <- httpbin$url("/stream-bytes/100", c(chunk_size = 30))
  resp <- curl::curl_fetch_memory(url)
  headers <- curl:::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 200)
  expect_equal(headers[["transfer-encoding"]], "chunked")
  expect_equal(length(resp$content), 100)
})

test_that("/uuid", {
  url <- httpbin$url("/uuid")
  resp <- curl::curl_fetch_memory(url)
  echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_match(echo$uuid, "^[-0-9a-z]+$")
  expect_equal(nchar(echo$uuid), 36)
})

# Cookies ==============================================================

test_that("/cookies", {
  url <- httpbin$url("/cookies")
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, Cookie = "foo=bar; bar=baz")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(data, list(cookies = list(foo = "bar", bar = "baz")))
})

test_that("/cookies/set/:name/:value", {
  url <- httpbin$url("/cookies/set/foo/bar")
  handle <- curl::new_handle()
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers(resp$headers, multiple = TRUE)
  expect_true("HTTP/1.1 302 Found" %in% headers[[1]])
  expect_true("Set-Cookie: foo=bar; Path=/" %in% headers[[1]])
  expect_true("HTTP/1.1 200 OK" %in% headers[[2]])
  expect_equal(resp$status_code, 200L)
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(data, list(cookies = list(foo = "bar")))
  expect_snapshot(curl::handle_cookies(handle), variant = r_variant())
})

test_that("/cookies/set", {
  url <- httpbin$url("/cookies/set?foo=bar&bar=baz")
  handle <- curl::new_handle()
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers(resp$headers, multiple = TRUE)
  expect_true("HTTP/1.1 302 Found" %in% headers[[1]])
  expect_true("Set-Cookie: foo=bar; Path=/" %in% headers[[1]])
  expect_true("Set-Cookie: bar=baz; Path=/" %in% headers[[1]])
  expect_true("HTTP/1.1 200 OK" %in% headers[[2]])
  expect_equal(resp$status_code, 200L)
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(data, list(cookies = list(bar="baz", foo = "bar")))
  expect_snapshot(curl::handle_cookies(handle), variant = r_variant())
})

test_that("/cookies/delete", {
  handle <- curl::new_handle()

  url1 <- httpbin$url("/cookies/set?foo=bar&bar=baz")
  resp1 <- curl::curl_fetch_memory(url1, handle = handle)

  url <- httpbin$url("/cookies/delete?foo")
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers(resp$headers, multiple = TRUE)
  expect_true("HTTP/1.1 302 Found" %in% headers[[1]])
  expect_true("Set-Cookie: foo=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Max-Age=0; Path=/" %in% headers[[1]])
  expect_true("HTTP/1.1 200 OK" %in% headers[[2]])
  expect_equal(resp$status_code, 200L)
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(data, list(cookies = list(bar = "baz")))
  expect_snapshot(curl::handle_cookies(handle), variant = r_variant())
})

# Images ===============================================================

test_that("/image", {
  url <- httpbin$url("/image")
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
    url <- httpbin$url(paste0("/image/", ext))
    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status_code, 200L)
    expect_equal(resp$type, unname(mime_find(ext)))
  }
})

# Redirects ============================================================

test_that("absolute-redirect", {
  url <- httpbin$url("/absolute-redirect/4")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 302L)
  expect_equal(headers$location, httpbin$url("/absolute-redirect/3"))

  url <- httpbin$url("/absolute-redirect/2")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = TRUE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$url, httpbin$url("/get"))

  url <- httpbin$url("/absolute-redirect/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
})

test_that("redirect", {
  url <- httpbin$url("/redirect/4")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(resp$headers)
  expect_equal(resp$status_code, 302L)
  expect_equal(headers$location, "/redirect/3")

  url <- httpbin$url("/redirect/2")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = TRUE)
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  expect_equal(resp$url, httpbin$url("/get"))

  url <- httpbin$url("/redirect/foo")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 404L)
})

test_that("/redirect-to", {
  # default status is 302
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  url <- httpbin$url("/redirect-to", query = list(url = "/get"))
  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 302L)
  expect_equal(resp$type, "text/plain")
  expect_equal(curl::parse_headers_list(resp$headers)$location, "/get")
  expect_equal(rawToChar(resp$content), "302 Found. Redirecting to /get")

  # can specify status
  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = FALSE)
  url <- httpbin$url(
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
