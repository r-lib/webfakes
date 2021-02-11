
test_that("PACKAGES files", {
  app <- cran_app()
  app$add_repo()
  cran <- local_app_process(app)

  packages <- cran$url("/CRAN/src/contrib/PACKAGES")
  resp <- curl::curl_fetch_memory(packages)

  on.exit(try_silently(close(rc)), add = TRUE)
  expect_silent(
    cnt <- read.dcf(rc <- rawConnection(resp$content))
  )

  packagesgz <- cran$url("/CRAN/src/contrib/PACKAGES.gz")
  resp <- curl::curl_fetch_memory(packagesgz)

  on.exit(try_silently(close(c1)), add = TRUE)
  on.exit(try_silently(close(c2)), add = TRUE)
  expect_silent(
    cnt2 <- read.dcf(c1 <- gzcon(c2 <- rawConnection(resp$content)))
  )

  packagesrds <- cran$url("/CRAN/src/contrib/PACKAGES.rds")
  resp <- curl::curl_fetch_memory(packagesrds)
  writeBin(resp$content, tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  expect_silent(cnt3 <- readRDS(tmp))

  expect_equal(cnt, cnt2)
  rownames(cnt) <- cnt[, "Package"]
  expect_equal(cnt, cnt3)

  expect_silent(
    ap <- utils::available.packages(
      repos = c(CRAN = cran$url("/CRAN")),
      ignore_repo_cache = TRUE
    )
  )
})

test_that("Can opt out of PACKAGES file types", {
  app <- cran_app()
  app$set_config("packages_files", c("PACKAGES", "PACKAGES.gz"))
  app$add_repo()
  cran <- local_app_process(app)

  packages <- cran$url("/CRAN/src/contrib/PACKAGES")
  resp <- curl::curl_fetch_memory(packages)
  expect_equal(resp$status_code, 200)

  packagesgz <- cran$url("/CRAN/src/contrib/PACKAGES.gz")
  resp <- curl::curl_fetch_memory(packagesgz)
  expect_equal(resp$status_code, 200)

  packagesrds <- cran$url("/CRAN/src/contrib/PACKAGES.rds")
  resp <- curl::curl_fetch_memory(packagesrds)
  expect_equal(resp$status_code, 404)
})
