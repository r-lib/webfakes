
httpbin2 <- local_app_process(
  httpbin_app(),
  .teardown_env = testthat::teardown_env()
)
