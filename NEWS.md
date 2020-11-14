
# Development version

# presser 1.1.0

* Support chunked resposes via the new `response$send_chunk()` method.

* New `httpbin_app()` end point: `/stream-bytes`, to send chunked
  random bytes.

* New `httpbin_app()` end point: `/response-headers`, to set response
  headers.

* The web server now does not crash if the connection is closed by the
  client while delaying the response.

# presser 1.0.0

First public release.
