# webfakes 1.3.1

No changes.

# webfakes 1.3.0

* New `git_app()` app to fake a git HTTP server. See the webfakes test cases
  for examples.

* New `mw_cgi()` middleware to call CGI scripts. See the new `git_app()`
  for an example.

# webfakes 1.2.1

* `tmpl_glue()` now works correctly on platforms with an issue in
  `readChar(..., useBytes = TRUE)`, e.g. on macOS 14.x Sonoma:
  <https://bugs.r-project.org/show_bug.cgi?id=18605>.

# webfakes 1.2.0

* The httpbin app now implements the `/brotli`, `/deflate`, `/digest-auth`
  `/forms/post`, `/hidden-basic-auth`, `/range/:n`, `/stream/:n`, `/cache`
  and `/cache/:value` endpoints. With these, it implements all endpoint of
  the original Python httpbin app (#3).

* New middleware `mw_cookie_parser()` to parse a `Cookie` header. Relatedly,
  new `response$add_cookie()` and `response$clear_cookie()` methods to add a
  cookie to a response and to add a header that clears a cookie (#2).

* Parsing query parametes without a value now does not fail.

* New utility function `http_time_stamp()` to format a time stamp for HTTP.

* The httpbin app now implements the endpoints related to cookies (#3).

* The httpbin app now sends the `Date` header in the correct format.

* The `offset` parameter is now optional in the `/links` endpoint of the
  httpbin app.

* `mw_etag()` now does not add an `ETag` header to the response, if there
  is one already. (The comparision is case sensitive.)

* New middleware: `mw_range_parser()` to parse `Range` headers.

# webfakes 1.1.7

* No user visible changes.

# webfakes 1.1.6

* `response$send_file()` now handles `root = "/"` and absolute paths
  better on Windows.

* `new_app_process()` and `local_app_process()` are now faster,
  because the app object they need to copy to the subprocess is smaller.

# webfakes 1.1.5

* `mw_etag()` now handles the `If-None-Match` header properly, and sets
  the status code of the response to 304, and removes the response body.

# webfakes 1.1.4

* No user visible changes.

# webfakes 1.1.3

* webfakes now compiles on older macOS versions, hopefully really.

# webfakes 1.1.2

* webfakes now compiles on older macOS versions (before 10.12).

# 1.1.1

First release on CRAN
