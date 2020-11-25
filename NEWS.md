
# Development version

* `mw_json()` and `mw_urlencoded()` now do not fail when the request
  body in `NULL`.

* New `local_app_process()` function to automatically clean up the
  server process.

* Now `new_app_process()` (and the new `local_app_process()` as well) only
  start the server process for frist `$url()` or `$get_port()` call,
  by default.

* Now `new_app_process()` and `local_app_process()` automatically clean up
  the server process is the main R process terminates.

# webfakes 1.1.0

* Support chunked resposes via the new `response$send_chunk()` method.

* New `httpbin_app()` end point: `/stream-bytes`, to send chunked
  random bytes.

* New `httpbin_app()` end point: `/response-headers`, to set response
  headers.

* The web server now does not crash if the connection is closed by the
  client while delaying the response.

# webfakes 1.0.0

First public release.
