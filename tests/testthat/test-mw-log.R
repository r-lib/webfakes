
web <- setup({
  app <- new_app()$
    use(mw_log())$
    get("/txt", function(req, res) {
      res$
        set_type("text/plain")$
        send("textual")
    })$
    get("/html", function(req, res) {
      res$
        set_type("text/html")$
        send("<html><head></head><body>hello</body></html>")
    })$
    get("/raw", function(req, res) {
      res$
        set_type("applicartion/octet-stream")$
        send(charToRaw("raw"))
    })
  new_app_process(app, callr_opts = list(stdout = "|"))
})

teardown(web$stop())

test_that("text/plain response", {
  url <- web$url("/txt")
  resp <- curl::curl_fetch_memory(url)
  plr <- web$.process$poll_io(1000)
  expect_equal(plr[["output"]], "ready")
  log <- web$.process$read_output_lines()
  expect_match(log, "GET http://127\\.0\\.0\\.1:[0-9]*/txt 200 [0-9]+ ms - 7")
})

test_that("text/html response", {
  url <- web$url("/html")
  resp <- curl::curl_fetch_memory(url)
  plr <- web$.process$poll_io(1000)
  expect_equal(plr[["output"]], "ready")
  log <- web$.process$read_output_lines()
  expect_match(log, "GET http://127\\.0\\.0\\.1:[0-9]+/html 200 [0-9]+ ms - 44")
})

test_that("application/octet-stream response", {
  url <- web$url("/raw")
  resp <- curl::curl_fetch_memory(url)
  plr <- web$.process$poll_io(1000)
  expect_equal(plr[["output"]], "ready")
  log <- web$.process$read_output_lines()
  expect_match(log, "GET http://127\\.0\\.0\\.1:[0-9]+/raw 200 [0-9]+ ms - 3")
})
