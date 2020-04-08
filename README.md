
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presser

> Lightweight Web Server for Testing

<!-- badges: start -->

[![R build
status](https://github.com/gaborcsardi/pressr/workflows/R-CMD-check/badge.svg)](https://github.com/gaborcsardi/pressr/actions)
[![](http://www.r-pkg.org/badges/version/presser)](http://www.r-pkg.org/pkg/presser)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/presser)](http://www.r-pkg.org/pkg/presser)
<!-- badges: end -->

Lightweight Web Server for Testing.

## Features

  - Complete web app framework, define handlers for HTTP request in R.
  - Flexible path matching, with parameters and regular expressions.
  - Built in templating system using glue or bring your own template
    engine.
  - Middleware to parse JSON, multipart and URL encoded request bodies.
  - A web app is just an R object. It can be saved to disk, copied to
    another R process, put in an R package, etc.
  - A web app is extensible, by adding new routes and middleware to it.
  - Helper functions for sending JSON, files from disk, etc.
  - Comes with an app similar to the <https://httpbin.org> API, so for
    simple tests you don’t need to write your own web app.

## Optional dependencies

  - The jsonlite package is needed for the `mw_json()` middleware an the
    `response$send_json()` method.
  - The glue package is needed for the `tmpl_glue()` template engine.
  - The digest package is needed for the `mw_etag()` middleware.
  - The callr package is needed for `new_app_process()` to work.
  - The digest and jsonlite packages are needed for the `httpbin_app()`
    app.
  - The uuid package is needed if you use the `/uuid` path of the the
    `httpbin_app()` app.

## Installation

Once on CRAN, install the package as usual:

``` r
install.packages("presser")
```

## Usage

Start a web app at the beginning of your tests or test file, and stop it
after. Here is an example with the testthat package. Suppose you want to
test that your `get_hello()` function can query an API:

``` r
web <- setup({
  app <- presser::new_app()
  app$get("/hello/:user", function(req, res) {
    res$send(paste0("Hello ", req$params$user, "!"))
  })
  presser::new_app_process(app)
})
teardown(web$stop())

test_that("can use hello API", {
  url <- web$get_url("/hello/Gabor")
  expect_equal(get_hello(url), "Hello Gabor!")
})
```

When testing HTTP clients you can often use the built in
`httpbin_app()`:

``` r
httpbin <- setup(presser::new_app_process(presser::httpbin_app()))
teardown(httpbin$stop())

test_that("HTTP errors are caught", {
  url <- httpbin$get_url("/status/404")
  resp <- httr::GET(url)
  expect_error(httr::stop_for_status(resp), class = "http_404")
})
```

## License

MIT © RStudio
