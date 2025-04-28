test_that("invalid handler", {
  app <- new_app()
  expect_snapshot(error = TRUE, {
    app$use("foobar")
    app$get("/foo", 1:100)
  })
})
