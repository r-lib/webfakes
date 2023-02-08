
<!-- README.md is generated from README.Rmd. Please edit that file -->

# webfakes

> Your own web server for happy HTTP testing

<!-- badges: start -->

[![R build
status](https://github.com/r-lib/webfakes/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/webfakes/actions)

[![CRAN
status](https://www.r-pkg.org/badges/version/webfakes)](https://CRAN.R-project.org/package=webfakes)

<!-- badges: end -->

Lightweight fake web apps for testing. Built using the
[civetweb](https://github.com/civetweb/civetweb) embedded web server.

## Features

-   Complete web app framework, define handlers for HTTP requests in R.
-   Write your own app for your custom test cases; our use app similar
    to the <https://httpbin.org> API, so often you don’t need to write
    your own web app (e.g. if you are writing an HTTP client (httr,
    curl, crul).
-   Run one web app per test suite, per test file or per test case.
-   Flexible path matching, with parameters and regular expressions.
-   Built in templating system using glue or bring your own template
    engine.
-   Middleware to parse JSON, multipart and URL encoded request bodies.
-   A web app is just an R object. It can be saved to disk, copied to
    another R process, etc.
-   A web app is extensible, by adding new routes and middleware to it.
-   Helper functions for sending JSON, files from disk, etc.
-   App-specific environment to store any data including data from
    requests to the fake app.
-   After a web app is launched from R, you can interact with it from R
    but also from the command line, your browser, etc. Nice for
    debugging.
-   The web server runs in the R process, so it has no problems with
    local firewalls.
-   Multi-threaded web server supports concurrent HTTP requests.
-   Limit download speed to simulate low bandwidth.

## Optional dependencies

-   The jsonlite package is needed for the `mw_json()` middleware, the
    `response$send_json()` method and the `httpbin_app()` app.
-   The glue package is needed for the `tmpl_glue()` template engine.
-   The callr package is needed for `new_app_process()` and
    `local_app_process` to work.

## Installation

Install the release version from CRAN:

``` r
install.packages("webfakes")
```

## Usage

Start a web app at the beginning of your tests or test file, and stop it
after. Here is an example with the testthat package. Suppose you want to
test that your `get_hello()` function can query an API:

`local_app_process()` helps you clean up the web server process after
the test block, or test file. It is similar to the `withr::local_*`
functions.

``` r
app <- webfakes::new_app()
app$get("/hello/:user", function(req, res) {
  res$send(paste0("Hello ", req$params$user, "!"))
})
web <- webfakes::local_app_process(app)

test_that("can use hello API", {
  url <- web$url("/hello/Gabor")
  expect_equal(get_hello(url), "Hello Gabor!")
})
```

When testing HTTP clients you can often use the built in
`httpbin_app()`:

``` r
httpbin <- webfakes::local_app_process(webfakes::httpbin_app())
```

``` r
test_that("HTTP errors are caught", {
  url <- httpbin$url("/status/404")
  resp <- httr::GET(url)
  expect_error(httr::stop_for_status(resp), class = "http_404")
})
```

    #> Test passed 🎉

## Documentation

See <https://webfakes.r-lib.org>

## Links

### Other solutions for HTTP testing in R:

-   [vcr](https://github.com/ropensci/vcr)
-   [httptest](https://github.com/nealrichardson/httptest)

### R web application frameworks

webfakes focuses on testing, these packages are for writing real web
apps:

-   [shiny](https://github.com/rstudio/shiny)
-   [opencpu](https://www.opencpu.org/)
-   [plumber](https://github.com/rstudio/plumber)
-   [fiery](https://github.com/thomasp85/fiery)
-   [RestRserve](https://github.com/rexyai/RestRserve)

## Code of Conduct

Please note that the fs project is released with a
[Contributor Code of Conduct](https://r-lib.github.io/webfakes/dev/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

MIT © RStudio
