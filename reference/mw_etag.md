# Middleware that add an `ETag` header to the response

If the response already has an `ETag` header, then it is kept.

## Usage

``` r
mw_etag(algorithm = "crc32")
```

## Arguments

- algorithm:

  Checksum algorithm to use. Only `"crc32"` is implemented currently.

## Value

Handler function.

## Details

This middleware handles the `If-None-Match` headers, and it sets the
status code of the response to 304 if `If-None-Match` matches the
`ETag`. It also removes the response body in this case.

## See also

Other middleware:
[`mw_cgi()`](https://webfakes.r-lib.org/reference/mw_cgi.md),
[`mw_cookie_parser()`](https://webfakes.r-lib.org/reference/mw_cookie_parser.md),
[`mw_json()`](https://webfakes.r-lib.org/reference/mw_json.md),
[`mw_log()`](https://webfakes.r-lib.org/reference/mw_log.md),
[`mw_multipart()`](https://webfakes.r-lib.org/reference/mw_multipart.md),
[`mw_range_parser()`](https://webfakes.r-lib.org/reference/mw_range_parser.md),
[`mw_raw()`](https://webfakes.r-lib.org/reference/mw_raw.md),
[`mw_static()`](https://webfakes.r-lib.org/reference/mw_static.md),
[`mw_text()`](https://webfakes.r-lib.org/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/reference/mw_urlencoded.md)

## Examples

``` r
app <- new_app()
app$use(mw_etag())
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
