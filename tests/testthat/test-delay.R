
app <- new_app()
app$get("/delay", function(req, res) {
  if (is.null(res$locals$seen)) {
    res$locals$seen <- TRUE
    res$delay(.5)
  } else {
    res$send_json(
      list(message = "Sorry, running late..."),
      auto_unbox = TRUE
    )
  }
})
app$get("/nodelay", function(req, res) {
  res$send_json(
    list(message = "I am fast, aren't I?"),
    auto_unbox = TRUE
  )
})

web <- local_app_process(app, opts = server_opts(num_threads = 2))

 test_that("delay", {
  p <- curl::new_pool(multiplex = FALSE)
  h1 <- curl::new_handle(url = web$url("/delay"))
  h2 <- curl::new_handle(url = web$url("/nodelay"))
  resp1 <- resp2 <- NULL
  curl::multi_add(h1, done = function(x) resp1 <<- x, fail = stop, pool = p)
  curl::multi_add(h2, done = function(x) resp2 <<- x, fail = stop, pool = p)
  curl::multi_run(timeout = 2, pool = p)
  curl::multi_cancel(h1)
  curl::multi_cancel(h2)

  expect_false(is.null(resp1))
  expect_false(is.null(resp2))
  expect_true(resp1$times[["total"]] > 0.5)
  expect_true(resp2$times[["total"]] < resp1$times[["total"]])
})
