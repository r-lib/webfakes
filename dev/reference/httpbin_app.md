# Generic web app for testing HTTP clients

A web app similar to `https://httpbin.org`. See [its specific
docs](https://webfakes.r-lib.org/httpbin.html). You can also see these
docs locally, by starting the app:

    httpbin <- new_app_process(httpbin_app())
    browseURL(httpbin$url())

## Usage

``` r
httpbin_app(log = interactive())
```

## Arguments

- log:

  Whether to log requests to the standard output.

## Value

A `webfakes_app`.

## Examples

``` r
app <- httpbin_app()
proc <- new_app_process(app)
url <- proc$url("/get")
resp <- curl::curl_fetch_memory(url)
curl::parse_headers_list(resp$headers)
#> $connection
#> [1] "close"
#> 
#> $date
#> [1] "Wed, 15 Apr 2026 07:25:01 GMT"
#> 
#> $`content-type`
#> [1] "application/json"
#> 
#> $`content-length`
#> [1] "313"
#> 
#> $etag
#> [1] "\"0f6c389b\""
#> 
cat(rawToChar(resp$content))
#> {
#>   "args": {},
#>   "headers": {
#>     "Host": "127.0.0.1:44707",
#>     "User-Agent": "R/4.5.3 R (4.5.3 x86_64-pc-linux-gnu x86_64 linux-gnu) on GitHub Actions",
#>     "Accept": "*/*",
#>     "Accept-Encoding": "deflate, gzip, br, zstd"
#>   },
#>   "origin": "127.0.0.1",
#>   "path": "/get",
#>   "url": "http://127.0.0.1:44707/get"
#> }
proc$stop()
```
