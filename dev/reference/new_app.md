# Create a new web application

Create a new web application

## Usage

``` r
new_app()
```

## Value

A new `webfakes_app`.

## Details

The typical workflow of creating a web application is:

1.  Create a `webfakes_app` object with `new_app()`.

2.  Add middleware and/or routes to it.

3.  Start is with the `webfakes_app$listen()` method, or start it in
    another process with
    [`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md).

4.  Make queries to the web app.

5.  Stop it via `CTRL+C` / `ESC`, or, if it is running in another
    process, with the `$stop()` method of
    [`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md).

A web application can be

- restarted,

- saved to disk,

- copied to another process using the callr package, or a similar way,

- embedded into a package,

- extended by simply adding new routes and/or middleware.

The webfakes API is very much influenced by the
[express.js](https://expressjs.com/) project.

### Create web app objects

    new_app()

`new_app()` returns a `webfakes_app` object the has the methods listed
on this page.

An app is an environment with S3 class `webfakes_app`.

### The handler stack

An app has a stack of handlers. Each handler can be a route or
middleware. The differences between the two are:

- A route is bound to one or more paths on the web server. Middleware is
  not (currently) bound to paths, but run for all paths.

- A route is usually (but not always) the end of the handler stack for a
  request. I.e. a route takes care of sending out the response to the
  request. Middleware typically performs some action on the request or
  the response, and then the next handler in the stack is invoked.

### Routes

The following methods define routes. Each method corresponds to the HTTP
verb with the same name, except for `app$all()`, which creates a route
for all HTTP methods.

    app$all(path, ...)
    app$delete(path, ...)
    app$get(path, ...)
    app$head(path, ...)
    app$patch(path, ...)
    app$post(path, ...)
    app$put(path, ...)
    ... (see list below)

- `path` is a path specification, see 'Path specification' below.

- `...` is one or more handler functions. These will be placed in the
  handler stack, and called if they match an incoming HTTP request. See
  'Handler functions' below.

webfakes also has methods for the less frequently used HTTP verbs:
`CONNECT`, `MKCOL`, `OPTIONS`, `PROPFIND`, `REPORT`. (The method names
are always in lowercase.)

If a request is not handled by any routes (or handler functions in
general), then webfakes will send a simple HTTP 404 response.

### Middleware

`app$use()` adds a middleware to the handler stack. A middleware is a
handler function, see 'Handler functions' below. webfakes comes with
middleware to perform common tasks:

- [`mw_cookie_parser()`](https://webfakes.r-lib.org/dev/reference/mw_cookie_parser.md)
  parses `Cookie` headers.

- [`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md)
  adds an `ETag` header to the response.

- [`mw_json()`](https://webfakes.r-lib.org/dev/reference/mw_json.md)
  parses JSON request bodies.

- [`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md) logs
  each requests to standard output, or another connection.

- [`mw_multipart()`](https://webfakes.r-lib.org/dev/reference/mw_multipart.md)
  parses multipart request bodies.

- [`mw_range_parser()`](https://webfakes.r-lib.org/dev/reference/mw_range_parser.md)
  parses `Range` headers.

- [`mw_raw()`](https://webfakes.r-lib.org/dev/reference/mw_raw.md)
  parses raw request bodies.

- [`mw_static()`](https://webfakes.r-lib.org/dev/reference/mw_static.md)
  serves static files from a directory.

- [`mw_text()`](https://webfakes.r-lib.org/dev/reference/mw_text.md)
  parses plain text request bodies.

- [`mw_urlencoded()`](https://webfakes.r-lib.org/dev/reference/mw_urlencoded.md)
  parses URL encoded request bodies.

    app$use(..., .first = FALSE)

- `...` is a set of (middleware) handler functions. They are added to
  the handler stack, and called for every HTTP request. (Unless an HTTP
  response is created before reaching this point in the handler stack.)

- `.first` set to `TRUE` is you want to add the handler function to the
  bottom of the stack.

### Handler functions

A handler function is a route or middleware. A handler function is
called by webfakes with the incoming HTTP request and the outgoing HTTP
response objects (being built) as arguments. The handler function may
query and modify the members of the request and/or the response object.
If it returns the string `"next"`, then it is *not* a terminal handler,
and once it returns, webfakes will move on to call the next handler in
the stack.

A typical route:

    app$get("/user/:id", function(req, res) {
      id <- req$params$id
      ...
      res$
        set_status(200L)$
        set_header("X-Custom-Header", "foobar")$
        send_json(response, auto_unbox = TRUE)
    })

- The handler belongs to an API path, which is a wildcard path in this
  case. It matches `/user/alice`, `/user/bob`, etc. The handler will be
  only called for GET methods and matching API paths.

- The handler receives the request (`req`) and the response (`res`).

- It sets the HTTP status, additional headers, and sends the data. (In
  this case the `webfakes_response$send_json()` method automatically
  converts `response` to JSON and sets the `Content-Type` and
  `Content-Length` headers.

- This is a terminal handler, because it does *not* return `"next"`.
  Once this handler function returns, webfakes will send out the HTTP
  response.

A typical middleware:

    app$use(function(req, res) {
      ...
      "next"
    })

- There is no HTTP method and API path here, webfakes will call the
  handler for each HTTP request.

- This is not a terminal handler, it does return `"next"`, so after it
  returns webfakes will look for the next handler in the stack.

### Errors

If a handler function throws an error, then the web server will return a
HTTP 500 `text/plain` response, with the error message as the response
body.

### Request and response objects

See
[webfakes_request](https://webfakes.r-lib.org/dev/reference/webfakes_request.md)
and
[webfakes_response](https://webfakes.r-lib.org/dev/reference/webfakes_response.md)
for the methods of the request and response objects.

### Path specification

Routes are associated with one or more API paths. A path specification
can be

- A "plain" (i.e. without parameters) string. (E.g. `"/list"`.)

- A parameterized string. (E.g. `"/user/:id"`.)

- A regular expression created via
  [`new_regexp()`](https://webfakes.r-lib.org/dev/reference/new_regexp.md)
  function.

- A list or character vector of the previous ones. (Regular expressions
  must be in a list.)

### Path parameters

Paths that are specified as parameterized strings or regular expressions
can have parameters.

For parameterized strings the keys may contain letters, numbers and
underscores. When webfakes matches an API path to a handler with a
parameterized string path, the parameters will be added to the request,
as `params`. I.e. in the handler function (and subsequent handler
functions, if the current one is not terminal), they are available in
the `req$params` list.

For regular expressions, capture groups are also added as parameters. It
is best to use named capture groups, so that the parameters are in a
named list.

If the path of the handler is a list of parameterized strings or regular
expressions, the parameters are set according to the first matching one.

### Templates

webfakes supports templates, using any template engine. It comes with a
template engine that uses the glue package, see
[`tmpl_glue()`](https://webfakes.r-lib.org/dev/reference/tmpl_glue.md).

`app$engine()` registers a template engine, for a certain file
extension. The `$render()` method of
[webfakes_response](https://webfakes.r-lib.org/dev/reference/webfakes_response.md)
can be called from the handler function to evaluate a template from a
file.

    app$engine(ext, engine)

- `ext`: the file extension for which the template engine is added. It
  should not contain the dot. E.g. `"html"', `"brew"\`.

- `engine`: the template engine, a function that takes the file path
  (`path`) of the template, and a list of local variables (`locals`)
  that can be used in the template. It should return the result.

An example template engine that uses glue might look like this:

    app$engine("txt", function(path, locals) {
      txt <- readChar(path, nchars = file.size(path))
      glue::glue_data(locals, txt)
    })

(The built-in
[`tmpl_glue()`](https://webfakes.r-lib.org/dev/reference/tmpl_glue.md)
engine has more features.)

This template engine can be used in a handler:

    app$get("/view", function(req, res) {
     txt <- res$render("test")
     res$
       set_type("text/plain")$
       send(txt)
    })

The location of the templates can be set using the `views` configuration
parameter, see the `$set_config()` method below.

In the template, the variables passed in as `locals`, and also the
response local variables (see `locals` in
[webfakes_response](https://webfakes.r-lib.org/dev/reference/webfakes_response.md)),
are available.

### Starting and stopping

    app$listen(port = NULL, opts = server_opts(), cleanup = TRUE)

- `port`: port to listen on. When `NULL`, the operating system will
  automatically select a free port. Add an `"s"` suffix to the port to
  use HTTPS. Use `"0s"` to use an OS assigned port with HTTPS. See the
  [how-to](https://webfakes.r-lib.org/dev/reference/how-to.md) manual
  page if you want to start the web server on more than one ports.

- `opts`: options to the web server. See
  [`server_opts()`](https://webfakes.r-lib.org/dev/reference/server_opts.md)
  for the list of options and their default values.

- `cleanup`: stop the server (with an error) if the standard input of
  the process is closed. This is handy when the app runs in a
  [`callr::r_session`](https://callr.r-lib.org/reference/r_session.html)
  subprocess, because it stops the app (and the subprocess) if the main
  process has terminated.

This method does not return, and can be interrupted with `CTRL+C` /
`ESC` or a SIGINT signal. See
[`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md)
for interrupting an app that is running in another process.

When `port` is `NULL`, the operating system chooses a port where the app
will listen. To be able to get the port number programmatically, before
the listen method blocks, it advertises the selected port in a
`webfakes_port` condition, so one can catch it:

webfakes by default binds only to the loopback interface at 127.0.0.1,
so the webfakes web app is never reachable from the network.

    withCallingHandlers(
      app$listen(),
      "webfakes_port" = function(msg) print(msg$port)
    )

### Logging

webfakes can write an access log that contains an entry for all incoming
requests, and also an error log for the errors that happen while the
server is running. This is the default behavior for local app (the ones
started by `app$listen()` and for remote apps (the ones started via
[`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md):

- Local apps do not write an access log by default.

- Remote apps write an access log into the
  `<tmpdir>/webfakes/<pid>/access.log` file, where `<tmpdir>` is the
  session temporary directory of the *main process*, and `<pid>` is the
  process id of the *subprocess*.

- Local apps write an error log to `<tmpdir>/webfakes/error.log`, where
  `<tmpdir>` is the session temporary directory of the current process.

- Remote app write an error log to the
  `<tmpdir>/webfakes/<pid>/error.log`, where `<tmpdir>` is the session
  temporary directory of the *main process* and `<pid>` is the process
  id of the *subprocess*\`.

See
[`server_opts()`](https://webfakes.r-lib.org/dev/reference/server_opts.md)
for changing the default logging behavior.

### Shared app data

    app$locals

It is often useful to share data between handlers and requests in an
app. `app$locals` is an environment that supports this. E.g. a
middleware that counts the number of requests can be implemented as:

    app$use(function(req, res) {
      locals <- req$app$locals
      if (is.null(locals$num)) locals$num <- 0L
      locals$num <- locals$num + 1L
      "next"
    })

[webfakes_response](https://webfakes.r-lib.org/dev/reference/webfakes_response.md)
objects also have a `locals` environment, that is initially populated as
a copy of `app$locals`.

### Configuration

    app$get_config(key)
    app$set_config(key, value)

- `key`: configuration key.

- `value`: configuration value.

Currently used configuration values:

- `views`: path where webfakes searches for templates.

## See also

[webfakes_request](https://webfakes.r-lib.org/dev/reference/webfakes_request.md)
for request objects,
[webfakes_response](https://webfakes.r-lib.org/dev/reference/webfakes_response.md)
for response objects.

## Examples

``` r
# see example web apps in the `/examples` directory in
system.file(package = "webfakes", "examples")
#> [1] "/home/runner/work/_temp/Library/webfakes/examples"

app <- new_app()
app$use(mw_log())

app$get("/hello", function(req, res) {
  res$send("Hello there!")
})

app$get(new_regexp("^/hi(/.*)?$"), function(req, res) {
  res$send("Hi indeed!")
})

app$post("/hello", function(req, res) {
  res$send("Got it, thanks!")
})

app
#> <webfakes_app>
#> routes:
#>   use *
#>   get /hello
#>   get <webfakes_regexp> "^/hi(/.*)?$"
#>   post /hello
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

# Start the app with: app$listen()
# Or start it in another R session: new_app_process(app)
```
