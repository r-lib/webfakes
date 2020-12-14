
test_that("cran_app", {
  app <- cran_app()
  app$add_repo()
  cran <- local_app_process(app)

  packages <- cran$url("/CRAN/src/contrib/PACKAGES")
  resp <- curl::curl_fetch_memory(packages)

  on.exit(try(close(rc)), add = TRUE)
  expect_silent(
    cnt <- read.dcf(rc <- rawConnection(resp$content))
  )
})
