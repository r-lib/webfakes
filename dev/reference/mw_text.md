# Middleware to parse a plain text body

Adds the parsed object as the `text` element of the request object.

## Usage

``` r
mw_text(default_charset = "utf-8", type = "text/plain")
```

## Arguments

- default_charset:

  Encoding to set on the text.

- type:

  Content type to match before parsing. If it does not match, then the
  request object is not modified.

## Value

Handler function.

## See also

Other middleware:
[`mw_cgi()`](https://webfakes.r-lib.org/dev/reference/mw_cgi.md),
[`mw_cookie_parser()`](https://webfakes.r-lib.org/dev/reference/mw_cookie_parser.md),
[`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md),
[`mw_json()`](https://webfakes.r-lib.org/dev/reference/mw_json.md),
[`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md),
[`mw_multipart()`](https://webfakes.r-lib.org/dev/reference/mw_multipart.md),
[`mw_range_parser()`](https://webfakes.r-lib.org/dev/reference/mw_range_parser.md),
[`mw_raw()`](https://webfakes.r-lib.org/dev/reference/mw_raw.md),
[`mw_static()`](https://webfakes.r-lib.org/dev/reference/mw_static.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/dev/reference/mw_urlencoded.md)

## Examples

``` r
app <- new_app()
app$use(mw_text())
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
