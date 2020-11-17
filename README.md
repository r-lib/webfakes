
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presser

> Your own web server for happy HTTP testing

<!-- badges: start -->

[![R build
status](https://github.com/gaborcsardi/pressr/workflows/R-CMD-check/badge.svg)](https://github.com/gaborcsardi/pressr/actions)

<!-- badges: end -->

Lightweight fake web apps for testing. Built using the
[civetweb](https://github.com/civetweb/civetweb) embedded web server.

## Features

  - Complete web app framework, define handlers for HTTP requests in R.
  - Write your own app for your custom test cases; our use app similar
    to the <https://httpbin.org> API, so often you don’t need to write
    your own web app (e.g. if you are writing an HTTP client (httr,
    curl, crul).
  - Run one web app per test suite, per test file or per test case.
  - Flexible path matching, with parameters and regular expressions.
  - Built in templating system using glue or bring your own template
    engine.
  - Middleware to parse JSON, multipart and URL encoded request bodies.
  - A web app is just an R object. It can be saved to disk, copied to
    another R process, etc.
  - A web app is extensible, by adding new routes and middleware to it.
  - Helper functions for sending JSON, files from disk, etc.
  - App-specific environment to store any data including data from
    requests to the fake app.
  - After a web app is launched from R, you can interact with it from R
    but also from the command line, your browser, etc. Nice for
    debugging.
  - The web server runs in the R process, so it has no problems with
    local firewalls.
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

    #> Test passed 😀

## Vocabulary

The presser package uses vocabulary that is standard for web apps,
especially those developed with Express.js, but not necessarily well
known to all R package developers.

**Apps** Central to this package are *apps* that are fake web services,
fake web APIs. When you define them they are not started, you start them
either by using the `$listen()` method, or by launching them in a new
process. See `?presser::new_app`. Until you start it an app is no more
than its specifications, and can be shared/saved to disk.

**Routes** As explained in [Express.js
docs](https://expressjs.com/en/guide/routing.html), “Routing refers to
how an application’s endpoints (URIs) respond to client requests.”. Each
route therefore is defined by a combination of HTTP methods (`get()`,
`post()`, etc. or `all()` for any HTTP method) and a path definition (a
string, parameterized string or regular expression), and by some code
using handler functions. If you don’t define any route, your app will
return 404 for all requests.

**Handler functions** Handler functions are what you use to parse the
request (query, body), to produce a response and, optionally, to change
data in the app’s local environment (so that further requests will have
access to it).

**Middleware functions** As explained in [Express.js
docs](https://expressjs.com/en/guide/writing-middleware.html),
“Middleware functions are functions that have access to the request
object (req), the response object (res), and the next function in the
application’s request-response cycle.” They are in the middle between
request and response. You have to view them as utility functions for the
whole app. In vocabulary close to usethis’ functions, when you write
`app$use(mw_raw())` the whole app will have access to the middleware
`mw_raw()`. There are built-in middleware functions in the package
(their name starts with `mw_`), and you can add your own.

**Handler stack** This is a fancy phrase to mention both routes and
middleware functions. Together they define *what* your web service does
*for what request* (routes, handler functions), with *what tools*
(middleware functions).

## Documentation

See <https://r-lib.github.io/presser/>

## License

MIT © RStudio
