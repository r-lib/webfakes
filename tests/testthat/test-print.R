
tmp <- setup({
  app <- new_app()
  app$use(function(req, res) {
    tmp <- tempfile()
    saveRDS(list(req = req, res = res), file = tmp)
    res$
      set_status(200)$
      send(normalizePath(tmp))
  })
  proc <- new_app_process(app)
  withr::local_options(list(HTTPUserAgent = "It is me, libcurl"))
  resp <- curl::curl_fetch_memory(proc$get_url())
  tmp <- rawToChar(resp$content)
  list(tmp = tmp, proc = proc)
})
teardown({ tmp$proc$stop(); unlink(tmp$tmp) })

test_that("presser_app", {
  verify_output(
    test_path("fixtures", "output", "presser_app.txt"),
    new_app()
  )
})

test_that("presser_request", {
  req <- readRDS(tmp$tmp)$req
  req$url <- "http://127.0.0.1:3000/"
  req$headers$host <- "127.0.0.1:3000"
  verify_output(
    test_path("fixtures", "output", "presser_request.txt"),
    req
  )
})

test_that("presser_response", {
  res <- readRDS(tmp$tmp)$res
  verify_output(
    test_path("fixtures", "output", "presser_response.txt"),
    res
  )
})

test_that("presser_regexp", {
  verify_output(
    test_path("fixtures", "output", "presser_regexp.txt"),
    new_regexp("^(foo|bar)$")
  )
})

test_that("presser_app_process", {
  app <- new_app()
  proc <- new_app_process(app)
  proc$stop()
  # make the output deterministic
  proc$.port <- 3000
  verify_output(
    test_path("fixtures", "output", "presser_app_process.txt"),
    proc
  )
})
