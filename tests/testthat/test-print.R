
app <- new_app()
app$use(function(req, res) {
  tmp <- tempfile()
  saveRDS(list(req = req, res = res), file = tmp)
  res$
    set_status(200)$
    send(normalizePath(tmp))
})
proc <- local_app_process(app)
withr::local_options(list(HTTPUserAgent = "It is me, libcurl"))
resp <- curl::curl_fetch_memory(proc$url())
tmp <- rawToChar(resp$content)

withr::defer(unlink(tmp))

# Verify_output uses a png() graphics device, and fails if there is
# no png() support. So we skip theses tests then. capabilities()
# is very slow on macOS, because it starts up X11, so we'll just assume
# that macOS has a png device.

skip_without_png_device <- function() {
  if (.Platform$OS.type == "windows") return()
  if (! capabilities("png") || ! capabilities("X11")) {
    skip("Needs a PNG device")
  }
}

test_that("webfakes_app", {
  skip_without_png_device()
  app <- new_app()
  app$use("add etag" = mw_etag())
  app$get("/api", function(req, res) res$send("foobar"))
  verify_output(
    test_path("fixtures", "output", "webfakes_app.txt"),
    app
  )
})

test_that("webfakes_request", {
  skip_without_png_device()
  req <- readRDS(tmp)$req
  req$url <- "http://127.0.0.1:3000/"
  req$headers$Host <- "127.0.0.1:3000"
  req$headers$`Accept-Encoding` <- "deflate, gzip"
  verify_output(
    test_path("fixtures", "output", "webfakes_request.txt"),
    req
  )
})

test_that("webfakes_response", {
  skip_without_png_device()
  res <- readRDS(tmp)$res
  verify_output(
    test_path("fixtures", "output", "webfakes_response.txt"),
    res
  )
})

test_that("webfakes_regexp", {
  skip_without_png_device()
  verify_output(
    test_path("fixtures", "output", "webfakes_regexp.txt"),
    new_regexp("^(foo|bar)$")
  )
})

test_that("webfakes_app_process", {
  skip_without_png_device()
  app <- new_app()
  proc <- new_app_process(app, start = TRUE)
  proc$stop()
  # make the output deterministic
  proc$.port <- 3000
  verify_output(
    test_path("fixtures", "output", "webfakes_app_process.txt"),
    proc
  )
})
