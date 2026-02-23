# Middleware that calls a CGI script

You can use it as an unconditional middleware in `app$use()`, as a
handler on `app$get()`, `app$post()`, etc., or you can call it from a
handler. See examples below.

## Usage

``` r
mw_cgi(command, args = character(), timeout = as.difftime(Inf, units = "secs"))
```

## Arguments

- command:

  External command to run.

- args:

  Arguments to pass to the external command.

- timeout:

  Timeout for the external command. If the command does not terminate in
  time, the web server kills it and returns an 500 response.

## Value

A function with signature

    function(req, res, env = character())

See [RFC 3875](https://datatracker.ietf.org/doc/html/rfc3875) for
details on the CGI protocol.

The request body (if any) is passed to the external command as standard
intput. `mw_cgi()` sets `CONTENT_LENGTH`, `CONTENT_TYPE`,
`GATEWAY_INTERFACE`, `PATH_INFO`, `QUERY_STRING`, `REMOTE_ADDR`,
`REMOTE_HOST`, `REMOTE_USER`, `REQUEST_METHOD`, `SERVER_NAME`,
`SERVER_PORT`, `SERVER_PROTOCOL`, `SERVER_SOFTEWARE`.

It does not currently set the `AUTH_TYPE`, `PATH_TRANSLATED`,
`REMOTE_IDENT`, `SCRIPT_NAME` environment variables.

The standard output of the external command is used to set the response
status code, the response headers and the response body. Example output
from git's CGI:

    Status: 200 OK
    Expires: Fri, 01 Jan 1980 00:00:00 GMT
    Pragma: no-cache
    Cache-Control: no-cache, max-age=0, must-revalidate
    Content-Type: application/x-git-upload-pack-advertisement

    000eversion 2
    0015agent=git/2.42.0
    0013ls-refs=unborn
    0020fetch=shallow wait-for-done
    0012server-option
    0017object-format=sha1
    0010object-info
    0000

## See also

Other middleware:
[`mw_cookie_parser()`](https://webfakes.r-lib.org/dev/reference/mw_cookie_parser.md),
[`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md),
[`mw_json()`](https://webfakes.r-lib.org/dev/reference/mw_json.md),
[`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md),
[`mw_multipart()`](https://webfakes.r-lib.org/dev/reference/mw_multipart.md),
[`mw_range_parser()`](https://webfakes.r-lib.org/dev/reference/mw_range_parser.md),
[`mw_raw()`](https://webfakes.r-lib.org/dev/reference/mw_raw.md),
[`mw_static()`](https://webfakes.r-lib.org/dev/reference/mw_static.md),
[`mw_text()`](https://webfakes.r-lib.org/dev/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/dev/reference/mw_urlencoded.md)

## Examples

``` r
app <- new_app()
app$use(mw_cgi("echo", "Status: 200\n\nHello"))
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

app2 <- new_app()
app2$get("/greet", mw_cgi("echo", "Status: 200\n\nHello"))
app2
#> <webfakes_app>
#> routes:
#>   get /greet
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

# Using `mw_cgi()` in a handler, you can pass extra environment variables
app3 <- new_app()
cgi <- mw_cgi("echo", "Status: 200\n\nHello")
app2$get("/greet", function(req, res) {
  cgi(req, res, env = c("EXTRA_VAR" = "EXTRA_VALUE"))
})
app3
#> <webfakes_app>
#> routes:
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
