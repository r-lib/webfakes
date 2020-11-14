
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presser

> Your own web server for happy HTTP testing

<!-- badges: start -->

[![R build
status](https://github.com/gaborcsardi/pressr/workflows/R-CMD-check/badge.svg)](https://github.com/gaborcsardi/pressr/actions)

<!-- badges: end -->

Lightweight web apps for testing. Built using the
[civetweb](https://github.com/civetweb/civetweb) embedded web server.

## Features

  - Complete web app framework, define handlers for HTTP request in R.
  - Flexible path matching, with parameters and regular expressions.
  - Built in templating system using glue or bring your own template
    engine.
  - Middleware to parse JSON, multipart and URL encoded request bodies.
  - A web app is just an R object. It can be saved to disk, copied to
    another R process, etc.
  - A web app is extensible, by adding new routes and middleware to it.
  - Helper functions for sending JSON, files from disk, etc.
  - Comes with an app similar to the <https://httpbin.org> API, so often
    you don’t need to write your own web app.
  - The web server runs in the R process, so it has no problems with
    local firewalls.
  - Write your own app for your custom test cases.
  - Run one web app per test suite, per test file or per test case.
  - Multi-threaded web server supports concurrent HTTP requests.
  - Limit download speed to simulate low bandwidth.

## Optional dependencies

  - The jsonlite package is needed for the `mw_json()` middleware, the
    `response$send_json()` method and the `httpbin_app()` app.
  - The glue package is needed for the `tmpl_glue()` template engine.
  - The callr package is needed for `new_app_process()` and
    `local_app_process` to work.

## Installation

Once on CRAN, install the package as usual:

``` r
install.packages("presser")
```

## Usage

Start a web app at the beginning of your tests or test file, and stop it
after. Here is an example with the testthat package. Suppose you want to
test that your `get_hello()` function can query an API:

`local_app_process()` helps you clean up the web server process after
the test block, or test file. It is similar to the `withr::local_*`
functions.

``` r
app <- presser::new_app()
app$get("/hello/:user", function(req, res) {
  res$send(paste0("Hello ", req$params$user, "!"))
})
web <- presser::local_app_process(app)

test_that("can use hello API", {
  url <- web$url("/hello/Gabor")
  expect_equal(get_hello(url), "Hello Gabor!")
})
```

When testing HTTP clients you can often use the built in
`httpbin_app()`:

``` r
httpbin <- presser::local_app_process(presser::httpbin_app())
```

``` r
test_that("HTTP errors are caught", {
  url <- httpbin$url("/status/404")
  resp <- httr::GET(url)
  expect_error(httr::stop_for_status(resp), class = "http_404")
})
```

    #> Test passed 🌈

## Documentation

See <https://r-lib.github.io/presser/>

## License

MIT © RStudio
