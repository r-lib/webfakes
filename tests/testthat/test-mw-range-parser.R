test_that("parse_range", {
  expect_snapshot({
    parse_range("foobar=1-100")
    parse_range("bytes=0-100, 50-150")
    parse_range("bytes=200-100")
    parse_range("bytes=x-100")

    parse_range("bytes=1-100")
    parse_range("bytes=1-")
    parse_range("bytes=-100")
    parse_range("bytes=0-100, 200-")
    parse_range("bytes=200-300, 0-100")
  })
})

test_that("intervals_overlap", {
  ok <- list(
    matrix(0, nrow = 0, ncol = 2),
    rbind(1:2),
    rbind(1:2, 3:4)
  )
  for (x in ok) {
    expect_false(intervals_overlap(x), info = x)
  }

  bad <- list(
    rbind(c(1, 10), c(5, 10)),
    rbind(c(1, 20), c(1, 20)),
    rbind(c(1, 1), c(1, 4))
  )
  for (x in bad) {
    expect_true(intervals_overlap(x), info = x)
  }
})
