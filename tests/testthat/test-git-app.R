test_that("git_app", {
  skip_on_cran()
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  untar(testthat::test_path("fixtures/git-repo.tar.gz"), exdir = tmp)
  app <- git_app(file.path(tmp, "repo"))
  git <- webfakes::local_app_process(app)

  expect_snapshot(
    system(
      paste("git ls-remote", git$url("/pak-test.git")),
      intern = TRUE
    )
  )
})
