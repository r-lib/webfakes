test_that("get", {
  url <- httpbin$url("/get")
  resp <- curl::curl_fetch_memory(url)
  expect_equal(resp$status_code, 200L)
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  expect_equal(data$path, "/get")
})

test_that("post", {
  url <- httpbin$url("/post")
  data <- charToRaw(jsonlite::toJSON(list(foo = "bar", foobar = 1:3)))
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "Content-Type" = "application/json")
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  expect_equal(resp$status_code, 200L)
  data <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(
    data$json,
    list(foo = "bar", foobar = 1:3)
  )
  expect_equal(data$path, "/post")
})

test_methods <- c(
  "connect",
  "delete",
  "head",
  "mkcol",
  "options",
  "patch",
  "propfind",
  "put",
  "report"
)

app <- new_app()
handler <- function(req, res) res$send_json(list(method = req$method))
for (method in test_methods) {
  app[[method]](paste0("/", method), handler)
}
web2 <- local_app_process(app, port = NA)

test_that("the rest", {
  for (method in test_methods) {
    url <- web2$url(paste0("/", method))
    handle <- curl::new_handle()
    curl::handle_setheaders(handle, "content-type" = "application/json")
    curl::handle_setopt(handle, customrequest = toupper(method))

    resp <- curl::curl_fetch_memory(url, handle = handle)
    expect_equal(resp$status, 200)
    if (method == "head") {
      expect_equal(length(resp$content), 0)
    } else {
      echo <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
      expect_equal(echo$method, method)
    }
  }
})
