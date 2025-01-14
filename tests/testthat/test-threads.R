test_that("parallel requests", {
  url <- httpbin$url("/delay/0.5")
  p <- curl::new_pool()
  handles <- replicate(3, curl::new_handle(url = url, http_version = 3))
  resps <- list()
  for (handle in handles) {
    curl::multi_add(
      handle,
      done = function(x) resps <<- c(resps, list(x)),
      fail = stop,
      pool = p
    )
  }
  st <- system.time(curl::multi_run(timeout = 5, pool = p))
  expect_true(st[["elapsed"]] >= 0.5)
  expect_true(st[["elapsed"]] < 1.5)
  expect_equal(resps[[1]]$status_code, 200L)
  expect_equal(resps[[2]]$status_code, 200L)
  expect_equal(resps[[3]]$status_code, 200L)
})
