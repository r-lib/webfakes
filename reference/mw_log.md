# Log requests to the standard output or other connection

A one line log entry for every request. The output looks like this:

    GET http://127.0.0.1:3000/image 200 3 ms - 4742

and contains

- the HTTP method,

- the full request URL,

- the HTTP status code of the response,

- how long it took to process the response, in ms,

- and the size of the response body, in bytes.

## Usage

``` r
mw_log(format = "dev", stream = "stdout")
```

## Arguments

- format:

  Log format. Not implemented currently.

- stream:

  R connection to log to. `"stdout"` means the standard output,
  `"stderr"` is the standard error. You can also supply a connection
  object, but then you need to be sure that it will be valid when the
  app is actually running.

## Value

Handler function.

## See also

Other middleware:
[`mw_cgi()`](https://webfakes.r-lib.org/reference/mw_cgi.md),
[`mw_cookie_parser()`](https://webfakes.r-lib.org/reference/mw_cookie_parser.md),
[`mw_etag()`](https://webfakes.r-lib.org/reference/mw_etag.md),
[`mw_json()`](https://webfakes.r-lib.org/reference/mw_json.md),
[`mw_multipart()`](https://webfakes.r-lib.org/reference/mw_multipart.md),
[`mw_range_parser()`](https://webfakes.r-lib.org/reference/mw_range_parser.md),
[`mw_raw()`](https://webfakes.r-lib.org/reference/mw_raw.md),
[`mw_static()`](https://webfakes.r-lib.org/reference/mw_static.md),
[`mw_text()`](https://webfakes.r-lib.org/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/reference/mw_urlencoded.md)

## Examples

``` r
app <- new_app()
app$use(mw_log())
app
#> <webfakes_app>
#> routes:
#>   use *
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
```
