# Changelog

## webfakes (development version)

No changes.

## webfakes 1.4.0

CRAN release: 2025-06-24

- webfakes now supports HTTPS
  ([\#110](https://github.com/r-lib/webfakes/issues/110)).

- The cleanup of a
  [`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md)
  (and thus
  [`local_app_process()`](https://webfakes.r-lib.org/dev/reference/local_app_process.md))
  is now faster. Instead of sending an interrupt first and waiting for
  the subprocess to quite, it is killed instantly.

## webfakes 1.3.2

CRAN release: 2025-01-11

- New server option: `decode_url`. If set to `FALSE`, then the web
  server will not URL-decode the URL
  ([\#106](https://github.com/r-lib/webfakes/issues/106)).

## webfakes 1.3.1

CRAN release: 2024-04-25

No changes.

## webfakes 1.3.0

CRAN release: 2023-12-11

- New [`git_app()`](https://webfakes.r-lib.org/dev/reference/git_app.md)
  app to fake a git HTTP server. See the webfakes test cases for
  examples.

- New [`mw_cgi()`](https://webfakes.r-lib.org/dev/reference/mw_cgi.md)
  middleware to call CGI scripts. See the new
  [`git_app()`](https://webfakes.r-lib.org/dev/reference/git_app.md) for
  an example.

## webfakes 1.2.1

CRAN release: 2023-10-01

- [`tmpl_glue()`](https://webfakes.r-lib.org/dev/reference/tmpl_glue.md)
  now works correctly on platforms with an issue in
  `readChar(..., useBytes = TRUE)`, e.g. on macOS 14.x Sonoma:
  <https://bugs.r-project.org/show_bug.cgi?id=18605>.

## webfakes 1.2.0

CRAN release: 2023-05-16

- The httpbin app now implements the `/brotli`, `/deflate`,
  `/digest-auth` `/forms/post`, `/hidden-basic-auth`, `/range/:n`,
  `/stream/:n`, `/cache` and `/cache/:value` endpoints. With these, it
  implements all endpoint of the original Python httpbin app
  ([\#3](https://github.com/r-lib/webfakes/issues/3)).

- New middleware
  [`mw_cookie_parser()`](https://webfakes.r-lib.org/dev/reference/mw_cookie_parser.md)
  to parse a `Cookie` header. Relatedly, new `response$add_cookie()` and
  `response$clear_cookie()` methods to add a cookie to a response and to
  add a header that clears a cookie
  ([\#2](https://github.com/r-lib/webfakes/issues/2)).

- Parsing query parametes without a value now does not fail.

- New utility function
  [`http_time_stamp()`](https://webfakes.r-lib.org/dev/reference/http_time_stamp.md)
  to format a time stamp for HTTP.

- The httpbin app now implements the endpoints related to cookies
  ([\#3](https://github.com/r-lib/webfakes/issues/3)).

- The httpbin app now sends the `Date` header in the correct format.

- The `offset` parameter is now optional in the `/links` endpoint of the
  httpbin app.

- [`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md) now
  does not add an `ETag` header to the response, if there is one
  already. (The comparision is case sensitive.)

- New middleware:
  [`mw_range_parser()`](https://webfakes.r-lib.org/dev/reference/mw_range_parser.md)
  to parse `Range` headers.

## webfakes 1.1.7

CRAN release: 2023-02-08

- No user visible changes.

## webfakes 1.1.6

CRAN release: 2022-11-08

- `response$send_file()` now handles `root = "/"` and absolute paths
  better on Windows.

- [`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md)
  and
  [`local_app_process()`](https://webfakes.r-lib.org/dev/reference/local_app_process.md)
  are now faster, because the app object they need to copy to the
  subprocess is smaller.

## webfakes 1.1.5

CRAN release: 2022-10-25

- [`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md) now
  handles the `If-None-Match` header properly, and sets the status code
  of the response to 304, and removes the response body.

## webfakes 1.1.4

CRAN release: 2022-09-08

- No user visible changes.

## webfakes 1.1.3

CRAN release: 2021-04-30

- webfakes now compiles on older macOS versions, hopefully really.

## webfakes 1.1.2

CRAN release: 2021-04-05

- webfakes now compiles on older macOS versions (before 10.12).
