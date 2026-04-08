# Happy HTTP testing with webfakes

## What is webfakes?

Webfakes is an R package that can spin up web servers on your machine to
facilitate testing R code. R code that needs an HTTP connection is not
trivial to test:

- Connectivity problems might prevent the tests from accessing the web
  server.
- The web server might need authentication, and it is not easy to convey
  login information to the test suite in a secure way.
- The web server might have rate limits, i.e, it limits the number of
  queries per hour or day, causing some spurious test failures.
- You might want to test in non-normal conditions, e.g. with low
  bandwidth, or when the client is rate limited. These conditions don’t
  normally happen on the web server and they are hard to trigger.

With webfakes you can easily start a custom web app, that is running on
the local machine.

- Webfakes does not need a network connection.
- Webfakes does not need authentication. Well, unless you want it to.
- Webfakes does not have rate limits.
- Webfakes can simulate low bandwidth, or a broken connection.

## Webfakes vs mocking

Mocking is a general technique to mimic the behavior of a function or
object that is needed in test case. In the case of HTTP requests, this
typically means that both the request and its response are recorded when
the tests run the first time, and saved to disk. Subsequent test runs
intercept the HTTP requests, match them against the recorded requests
and then replay the corresponding recorded response. See for example the
[vcr](https://docs.ropensci.org/vcr/) and
[httptest](https://enpiar.com/r/httptest/) R packages.

The advantages of using your own webfakes server, over mocking:

- Simpler infrastructure. No separate recording and replaying phases, no
  recorded files. No request matching.
- You can use any web client you want. E.g. curl and base R’s HTTP
  functions do not explicitly support mocking currently.
- No need to worry about sensitive information in recorded requests or
  responses.
- Often easier to use when testing non-normal conditions, e.g. errors
  that are hard to trigger, low bandwidth, or rate limits.
- Works if you stream data from a HTTP connection, instead of reading
  the whole response at once.
- You can reuse the same app for multiple tests, in multiple packages.
- Easier to use for tests that require multiple rounds of requests.
- Comes with a built-in `https://httpbin.org` compatible app, chances
  are, you don’t even need to write your testing app, just start writing
  tests right away.
- Better test writing experience. This is subjective, and your mileage
  may vary.

## Webfakes vs the real API

- No network needed. No more `skip_if_offline()`.
- Much faster.
- No rate limits. But you can simulate one if you want to.
- You can write your custom app.
- Simulate low bandwidth or a broken connection.

## Webfakes vs httpbin.org

- No network needed. No more `skip_if_offline()`.
- Much faster.
- You can use the built-in
  [`webfakes::httpbin_app()`](https://webfakes.r-lib.org/dev/reference/httpbin_app.md)
  app, so it is easy to switch from httpbin.org.
- You can write your custom app, httpbin.org might not have what you
  need.

## Using `webfakes::httpbin_app()` with testthat

You can use testthat’s setup files. You start the app in a setup file
and also register a teardown expression for it.
[`local_app_process()`](https://webfakes.r-lib.org/dev/reference/local_app_process.md)
can do both in one go. Your `tests/testthat/setup-http.R` may look like
this:

``` r
http <- webfakes::local_app_process(
  webfakes::httpbin_app(),
  .local_envir = testthat::teardown_env()
)
```

(Before testthat 3.0.0, you had to write the teardown expression in a
`tests/testthat/teardown-http.R` file. That still works, but a single
setup file is considered to be better practice, see [this testthat
vignette](https://testthat.r-lib.org/articles/test-fixtures.html).)

In the test cases you can query the `http` app process to get the URLs
you need to connect to:

``` r
test_that("fails on 404", {
  url <- http$url("/status/404")
  response <- httr::GET(url)
  expect_error(
    httr::stop_for_status(response),
    class = "http_404"
  )
})
#> Test passed with 1 success 🥇.
```

When writing your tests interactively, you may create a `http` app
process in the global environment, for convenience. You can
[`source()`](https://rdrr.io/r/base/source.html) your `setup-http.R`
file for this. Alternatively, you can start the app process in a helper
file. See “How do I start the app when writing the tests?” just below.

You can also create a web server for a test file, or even for a single
test case. See
[`vignette("how-to")`](https://webfakes.r-lib.org/dev/articles/how-to.md)
for details how.

## Writing apps

If the builtin
[`httpbin_app()`](https://webfakes.r-lib.org/dev/reference/httpbin_app.md)
is not appropriate for your tests, you can write your own app. You can
also extend the
[`httpbin_app()`](https://webfakes.r-lib.org/dev/reference/httpbin_app.md)
app, if you don’t want to start from scratch.

You create a new app with
[`new_app()`](https://webfakes.r-lib.org/dev/reference/new_app.md). This
returns an object with methods to add middleware and API endpoints to
it. For example, a simple app that returns the current time in JSON
would look like this:

``` r
time <- webfakes::new_app()
time$get("/time", function(req, res) {
  res$send_json(list(time = format(Sys.time())), auto_unbox = TRUE)
})
```

Now you can start this app on a random port using `web$listen()`.
Alternatively, you can start it in a subprocess with
[`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md).

``` r
web <- webfakes::new_app_process(time)
web$url()
#> [1] "http://127.0.0.1:44099/"
```

Use `web$url()` to query the URL of the app. For example:

``` r
url <- web$url("/time")
httr::content(httr::GET(url))
#> $time
#> [1] "2026-04-08 08:47:45"
```

`web$stop()` stops the app and the subprocess as well:

``` r
web$stop()
web$get_state()
#> [1] "not running"
```

[`local_app_process()`](https://webfakes.r-lib.org/dev/reference/local_app_process.md)
is similar to
[`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md),
but it stops the server process at the end of the calling block. This
means that the process is automatically cleaned up at the end of a
`test_that()` block or at the end of the test file.

You can create your app at the beginning of your test file. Or, if you
want to use the same app in multiple test files, use a [testthat helper
file](https://testthat.r-lib.org/reference/test_file.html#special-files).
Sometimes it useful if your users can create and use your test app, for
example to create reproducible examples. You can include a (possibly
internal) function in your package, that creates the app.

See `?new_app()`, `?new_app_process()` and
[`?local_app_process`](https://webfakes.r-lib.org/dev/reference/local_app_process.md)
for more details.
