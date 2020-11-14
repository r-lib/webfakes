
httpbin2 <- local_app_process(
  httpbin_app(),
  .local_envir = testthat::teardown_env()
)
