test_that("uuid_random() format", {
  uu <- replicate(1000, uuid_random())

  # format is right
  regex <- paste0(
    "^[0-9a-f]{8}-",
    "[0-9a-f]{4}-",
    "4[0-9a-f]{3}-",
    "[89ab][0-9a-f]{3}-",
    "[0-9a-f]{12}$"
  )
  expect_true(all(grepl(regex, uu, perl = TRUE)))

  # all bits are used, except for the dashes and M and N
  pos <- setdiff(1:36, c(9, 14, 19, 24, 15, 20))
  xd <- format(as.hexmode(0:15))
  for (p in pos) expect_true(all(xd %in% substr(uu, p, p)))

  # all bits are used for N
  expect_true(all(c("8", "9", "a", "b") %in% substr(uu, 20, 20)))
})
