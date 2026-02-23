# Run a webfakes app in another process

Runs an app in a subprocess, using
[callr::r_session](https://callr.r-lib.org/reference/r_session.html).

## Usage

``` r
new_app_process(
  app,
  port = NULL,
  opts = server_opts(remote = TRUE),
  start = FALSE,
  auto_start = TRUE,
  process_timeout = NULL,
  callr_opts = NULL
)
```

## Arguments

- app:

  `webfakes_app` object, the web app to run.

- port:

  Port(s) to use. By default the OS assigns a port. Add an `"s"` suffix
  to the port to use HTTPS. Use `"0s"` to use an OS assigned port with
  HTTPS. See the
  [how-to](https://webfakes.r-lib.org/dev/reference/how-to.md) on how to
  run the web server on multiple ports.

- opts:

  Server options. See
  [`server_opts()`](https://webfakes.r-lib.org/dev/reference/server_opts.md)
  for the defaults.

- start:

  Whether to start the web server immediately. If this is `FALSE`, and
  `auto_start` is `TRUE`, then it is started as neeed.

- auto_start:

  Whether to start the web server process automatically. If `TRUE` and
  the process is not running, then `$start()`, `$get_port()`,
  `$get_ports()` and `$url()` start the process.

- process_timeout:

  How long to wait for the subprocess to start, in milliseconds.

- callr_opts:

  Options to pass to
  [`callr::r_session_options()`](https://callr.r-lib.org/reference/r_session_options.html)
  when setting up the subprocess.

## Value

A `webfakes_app_process` object.

### Methods

The `webfakes_app_process` class has the following methods:

    get_app()
    get_port()
    get_ports()
    stop()
    get_state()
    local_env(envvars)
    url(path = "/", query = NULL)

- `envvars`: Named list of environment variables. The `{url}` substring
  is replaced by the URL of the app.

- `path`: Path to return the URL for.

- `query`: Additional query parameters, a named list, to add to the URL.

`get_app()` returns the app object.

`get_port()` returns the (first) port the web server is running on.

`get_ports()` returns all ports the web server is running on, and
whether it uses SSL on those ports, in a data frame with columns `ipv4`,
`ipv6`, `port` and `ssl`.

[`stop()`](https://rdrr.io/r/base/stop.html) stops the web server, and
also the subprocess. If the error log file is not empty, then it dumps
its contents to the screen.

`get_state()` returns a string, the state of the web server:

- `"not running"` the server is not running (because it was stopped
  already).

- `"live"` means that the server is running.

- `"dead"` means that the subprocess has quit or crashed.

`local_env()` sets the given environment variables for the duration of
the app process. It resets them in `$stop()`. Webfakes replaces `{url}`
in the value of the environment variables with the app URL, so you can
set environment variables that point to the app.

[`url()`](https://rdrr.io/r/base/connections.html) returns the URL of
the web app. You can use the `path` parameter to return a specific path.

## See also

[`local_app_process()`](https://webfakes.r-lib.org/dev/reference/local_app_process.md)
for automatically cleaning up the subprocess.

## Examples

``` r
app <- new_app()
app$get("/foo", function(req, res) {
  res$send("Hello world!")
})

proc <- new_app_process(app)
url <- proc$url("/foo")
resp <- curl::curl_fetch_memory(url)
cat(rawToChar(resp$content))
#> Hello world!

proc$stop()
```
