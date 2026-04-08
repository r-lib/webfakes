# Middleware to read the raw body of a request

Adds the raw body, as a raw object to the `raw` field of the request.

## Usage

``` r
mw_raw(type = "application/octet-stream")
```

## Arguments

- type:

  Content type to match. If it does not match, then the request object
  is not modified.

## Value

Handler function.

## See also

Other middleware:
[`mw_cgi()`](https://webfakes.r-lib.org/reference/mw_cgi.md),
[`mw_cookie_parser()`](https://webfakes.r-lib.org/reference/mw_cookie_parser.md),
[`mw_etag()`](https://webfakes.r-lib.org/reference/mw_etag.md),
[`mw_json()`](https://webfakes.r-lib.org/reference/mw_json.md),
[`mw_log()`](https://webfakes.r-lib.org/reference/mw_log.md),
[`mw_multipart()`](https://webfakes.r-lib.org/reference/mw_multipart.md),
[`mw_range_parser()`](https://webfakes.r-lib.org/reference/mw_range_parser.md),
[`mw_static()`](https://webfakes.r-lib.org/reference/mw_static.md),
[`mw_text()`](https://webfakes.r-lib.org/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/reference/mw_urlencoded.md)

## Examples

``` r
app <- new_app()
app$use(mw_raw())
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
