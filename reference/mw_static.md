# Middleware function to serve static files

The content type of the response is set automatically from the extension
of the file. Note that this is a terminal middleware handler function.
If a file is served, then the rest of the handler functions will not be
called. If a file was not found, however, the rest of the handlers are
still called.

## Usage

``` r
mw_static(root, set_headers = NULL)
```

## Arguments

- root:

  Root path of the served files. Everything under this directory is
  served automatically. Directory lists are not currently supports.

- set_headers:

  Callback function to call before a file is served.

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
[`mw_raw()`](https://webfakes.r-lib.org/reference/mw_raw.md),
[`mw_text()`](https://webfakes.r-lib.org/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/reference/mw_urlencoded.md)

## Examples

``` r
root <- system.file(package = "webfakes", "examples", "static", "public")
app <- new_app()
app$use(mw_static(root = root))
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
