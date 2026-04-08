# A webfakes request object

webfakes creates a `webfakes_request` object for every incoming HTTP
request. This object is passed to every matched route and middleware,
until the response is sent. It has reference semantics, so handlers can
modify it.

## Details

Fields and methods:

- `app`: The `webfakes_app` object itself.

- `headers`: Named list of HTTP request headers.

- `hostname`: The Host header, the server hostname and maybe port.

- `method`: HTTP method.

- `path`: Server path.

- `protocol`: `"http"` or `"https"`.

- `query_string`: The raw query string, without the starting `?`.

- `query`: Parsed query parameters in a named list.

- `remote_addr`: String, the domain name or IP address of the client.
  webfakes runs on the localhost, so this is `127.0.0.1`.

- `url`: The full URL of the request.

- `get_header(field)`: Function to query a request header. Returns
  `NULL` if the header is not present.

Body parsing middleware adds additional fields to the request object.
See [`mw_raw()`](https://webfakes.r-lib.org/reference/mw_raw.md),
[`mw_text()`](https://webfakes.r-lib.org/reference/mw_text.md),
[`mw_json()`](https://webfakes.r-lib.org/reference/mw_json.md),
[`mw_multipart()`](https://webfakes.r-lib.org/reference/mw_multipart.md)
and
[`mw_urlencoded()`](https://webfakes.r-lib.org/reference/mw_urlencoded.md).

## See also

[webfakes_response](https://webfakes.r-lib.org/reference/webfakes_response.md)
for the webfakes response object.

## Examples

``` r
# This is how you can see the request and response objects:
app <- new_app()
app$get("/", function(req, res) {
  browser()
  res$send("done")
})
app
#> <webfakes_app>
#> routes:
#>   get /
#> fields and methods:
#>   all(path, ...)         # add route for *all* HTTP methods
#>   delete(path, ...)      # add route for DELETE
#>   engine(ext, engine)    # add template engine for file extension
#>   head(path, ...)        # add route for HEAD
#>   listen(port)           # start web app on port
#>   patch(path, ...)       # add route for PATCH
#>   post(path, ...)        # add route for POST
#>   put(path, ...)         # add route for PUT
#>   use(...)               # add middleware
#>   locals                 # app-wide shared data
#> # see ?webfakes_app for all methods

# Now start this app on a port:
# app$listen(3000)
# and connect to it from a web browser: http://127.0.0.1:3000
# You can also use another R session to connect:
# httr::GET("http://127.0.0.1:3000")
# or the command line curl tool:
# curl -v http://127.0.0.1:3000
# The app will stop while processing the request.
```
