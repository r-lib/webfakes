# webfakes (development version)

* New middleware `mw_cookie_parser()` to parse a `Cookie` header. Relatedly,
  new `response$add_cookie()` method to add a cookie to a response.

* New utility function `http_time_stamp()` to format a time stamp for HTTP.

* The httpbin app now implements the `/cache` and `/cache/:value` endpoints.

* The httpbin app now sends the `Date` header in the correct format.

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
