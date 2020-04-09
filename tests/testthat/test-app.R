
test_that("invalid handler", {
  app <- new_app()
  expect_error(
    app$use("foobar"),
    "Invalid presser handler"
  )
  expect_error(
    app$get("/foo", 1:100),
    "Invalid presser handler"
  )
})
