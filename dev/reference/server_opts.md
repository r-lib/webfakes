# Webfakes web server options

Webfakes web server options

## Usage

``` r
server_opts(
  remote = FALSE,
  port = NULL,
  num_threads = 1,
  interfaces = "127.0.0.1",
  enable_keep_alive = FALSE,
  access_log_file = remote,
  error_log_file = TRUE,
  tcp_nodelay = FALSE,
  throttle = Inf,
  decode_url = TRUE,
  ssl_certificate = NULL
)
```

## Arguments

- remote:

  Meta-option. If set to `TRUE`, webfakes uses slightly different
  defaults, that are more appropriate for a background server process.

- port:

  Port to start the web server on. Defaults to a randomly chosen port.

- num_threads:

  Number of request handler threads to use. Typically you don't need
  more than one thread, unless you run test cases in parallel or you
  make concurrent HTTP requests.

- interfaces:

  The network interfaces to listen on. Being a test web server, it
  defaults to the localhost. Only bind to a public interface if you know
  what you are doing. webfakes was not designed to serve public web
  pages.

- enable_keep_alive:

  Whether the server keeps connections alive.

- access_log_file:

  `TRUE`, `FALSE`, or a path. See 'Logging' below.

- error_log_file:

  `TRUE`, `FALSE`, or a path. See 'Logging' below.

- tcp_nodelay:

  if `TRUE` then packages will be sent as soon as possible, instead of
  waiting for a full buffer or timeout to occur.

- throttle:

  Limit download speed for clients. If not `Inf`, then it is the maximum
  number of bytes per second, that is sent to as connection.

- decode_url:

  Whether the server should automatically decode URL-encodded URLs. If
  `TRUE` (the default), `/foo%2fbar` will be converted to `/foo/bar`
  automatically. If `FALSE`, URLs as not URL-decoded.

- ssl_certificate:

  Path to the SSL certificate of the server, needed if you want to
  server HTTPS requests.

## Value

List of options that can be passed to `webfakes_app$listen()` (see
[`new_app()`](https://webfakes.r-lib.org/dev/reference/new_app.md)), and
[`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md).

## Logging

- For `access_log_file`, `TRUE` means `<log-dir>/access.log`.

- For `error_log_file`, `TRUE` means `<log-dir>/error.log`.

`<log-dir>` is set to the contents of the `WEBFAKES_LOG_DIR` environment
variable, if it is set. Otherwise it is set to `<tmpdir>/webfakes` for
local apps and `<tmpdir>/<pid>/webfakes` for remote apps (started with
`new_app_procss()`).

`<tmpdir>` is the session temporary directory of the *main process*.

`<pid>` is the process id of the subprocess.

## Examples

``` r
# See the defaults
server_opts()
#> $remote
#> [1] FALSE
#> 
#> $port
#> NULL
#> 
#> $num_threads
#> [1] 1
#> 
#> $interfaces
#> [1] "127.0.0.1"
#> 
#> $enable_keep_alive
#> [1] FALSE
#> 
#> $access_log_file
#> [1] NA
#> 
#> $error_log_file
#> [1] "/tmp/Rtmpa0d0sf/webfakes/error.log"
#> 
#> $tcp_nodelay
#> [1] FALSE
#> 
#> $throttle
#> [1] Inf
#> 
#> $decode_url
#> [1] TRUE
#> 
#> $ssl_certificate
#> [1] "/home/runner/work/_temp/Library/webfakes/cert/localhost/server.pem"
#> 
```
