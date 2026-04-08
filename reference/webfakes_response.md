# A webfakes response object

webfakes creates a `webfakes_response` object for every incoming HTTP
request. This object is passed to every matched route and middleware,
until the HTTP response is sent. It has reference semantics, so handlers
can modify it.

## Details

Fields and methods:

- `app`: The `webfakes_app` object itself.

- `req`: The request object.

- `headers_sent`: Whether the response headers were already sent out.

- `locals`: Local variables, the are shared between the handler
  functions. This is for the end user, and not for the middlewares.

- `delay(secs)`: delay the response for a number of seconds. If a
  handler calls `delay()`, the same handler will be called again, after
  the specified number of seconds have passed. Use the `locals`
  environment to distinguish between the calls. If you are using
  `delay()`, and want to serve requests in parallel, then you probably
  need a multi-threaded server, see
  [`server_opts()`](https://webfakes.r-lib.org/reference/server_opts.md).

- `add_header(field, value)`: Add a response header. Note that
  `add_header()` may create duplicate headers. You usually want
  `set_header()`.

- `get_header(field)`: Query the currently set response headers. If
  `field` is not present it return `NULL`.

- `on_response(fun)`: Run the `fun` handler function just before the
  response is sent out. At this point the headers and the body are
  already properly set.

- `redirect(path, status = 302)`: Send a redirect response. It sets the
  `Location` header, and also sends a `text/plain` body.

- `render(view, locals = list())`: Render a template page. Searches for
  the `view` template page, using all registered engine extensions, and
  calls the first matching template engine. Returns the filled template.

- `send(body)`. Send the specified body. `body` can be a raw vector, or
  HTML or other text. For raw vectors it sets the content type to
  `application/octet-stream`.

- `send_json(object = NULL, text = NULL, ...)`: Send a JSON response.
  Either `object` or `text` must be given. `object` will be converted to
  JSON using
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
  `...` are passed to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
  It sets the content type appropriately.

- `send_file(path, root = ".")`: Send a file. Set `root = "/"` for
  absolute file names. It sets the content type automatically, based on
  the extension of the file, if it is not set already.

- `send_status(status)`: Send the specified HTTP status code, without a
  response body.

- `send_chunk(data)`: Send a chunk of a response in chunked encoding.
  The first chunk will automatically send the HTTP response headers.
  Webfakes will automatically send a final zero-lengh chunk, unless
  `$delay()` is called.

- `set_header(field, value)`: Set a response header. If the headers have
  been sent out already, then it throws a warning, and does nothing.

- `set_status(status)`: Set the response status code. If the headers
  have been sent out already, then it throws a warning, and does
  nothing.

- `set_type(type)`: Set the response content type. If it contains a `/`
  character then it is set as is, otherwise it is assumed to be a file
  extension, and the corresponding MIME type is set. If the headers have
  been sent out already, then it throws a warning, and does nothing.

- `add_cookie(name, value, options)`: Adds a cookie to the response.
  `options` is a named list, and may contain:

  - `domain`: Domain name for the cookie, not set by default.

  - `expires`: Expiry date in GMT. It must be a POSIXct object, and will
    be formatted correctly.

  - 'http_only': if TRUE, then it sets the 'HttpOnly' attribute, so
    Javasctipt cannot access the cookie.

  - `max_age`: Maximum age, in number of seconds.

  - `path`: Path for the cookie, defaults to "/".

  - `same_site`: The 'SameSite' cookie attribute. Possible values are
    "strict", "lax" and "none".

  - `secure`: if TRUE, then it sets the 'Secure' attribute.

- `clear_cookie(name, options = list())`: clears a cookie. Typically,
  web browsers will only clear a cookie if the options also match.

- `write(data)`: writes (part of) the body of the response. It also
  sends out the response headers, if they haven't been sent out before.

Usually you need one of the `send()` methods, to send out the HTTP
response in one go, first the headers, then the body.

Alternatively, you can use `$write()` to send the response in parts.

## See also

[webfakes_request](https://webfakes.r-lib.org/reference/webfakes_request.md)
for the webfakes request object.

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
