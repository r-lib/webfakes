# webfakes (development version)

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
