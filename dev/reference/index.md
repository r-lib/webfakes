# Package index

## Create and run web apps

- [`new_app()`](https://webfakes.r-lib.org/dev/reference/new_app.md) :
  Create a new web application
- [`webfakes_request`](https://webfakes.r-lib.org/dev/reference/webfakes_request.md)
  : A webfakes request object
- [`webfakes_response`](https://webfakes.r-lib.org/dev/reference/webfakes_response.md)
  : A webfakes response object
- [`new_regexp()`](https://webfakes.r-lib.org/dev/reference/new_regexp.md)
  : Create a new regular expression to use in webfakes routes
- [`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md)
  : Run a webfakes app in another process
- [`local_app_process()`](https://webfakes.r-lib.org/dev/reference/local_app_process.md)
  : App process that is cleaned up automatically
- [`server_opts()`](https://webfakes.r-lib.org/dev/reference/server_opts.md)
  : Webfakes web server options

## Middleware to parse requests

- [`mw_cookie_parser()`](https://webfakes.r-lib.org/dev/reference/mw_cookie_parser.md)
  : Middleware to parse Cookies
- [`mw_raw()`](https://webfakes.r-lib.org/dev/reference/mw_raw.md) :
  Middleware to read the raw body of a request
- [`mw_text()`](https://webfakes.r-lib.org/dev/reference/mw_text.md) :
  Middleware to parse a plain text body
- [`mw_json()`](https://webfakes.r-lib.org/dev/reference/mw_json.md) :
  Middleware to parse a JSON body
- [`mw_multipart()`](https://webfakes.r-lib.org/dev/reference/mw_multipart.md)
  : Parse a multipart HTTP request body
- [`mw_range_parser()`](https://webfakes.r-lib.org/dev/reference/mw_range_parser.md)
  : Middleware to parse a Range header
- [`mw_urlencoded()`](https://webfakes.r-lib.org/dev/reference/mw_urlencoded.md)
  : Middleware to parse an url-encoded request body

## Other middleware

- [`mw_cgi()`](https://webfakes.r-lib.org/dev/reference/mw_cgi.md) :
  Middleware that calls a CGI script

- [`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md) :

  Middleware that add an `ETag` header to the response

- [`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md) : Log
  requests to the standard output or other connection

- [`mw_static()`](https://webfakes.r-lib.org/dev/reference/mw_static.md)
  : Middleware function to serve static files

## Templates

- [`tmpl_glue()`](https://webfakes.r-lib.org/dev/reference/tmpl_glue.md)
  : glue based template engine

## The httpbin app

- [`httpbin_app()`](https://webfakes.r-lib.org/dev/reference/httpbin_app.md)
  : Generic web app for testing HTTP clients

## OAuth2.0 apps

- [`oauth2_httr_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_httr_login.md)
  : Helper function to use httr's OAuth2.0 functions non-interactively,
  e.g. in test cases
- [`oauth2_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_login.md)
  : Helper function to log in to a third party OAuth2.0 app without a
  browser
- [`oauth2_resource_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_resource_app.md)
  : Fake OAuth 2.0 resource and authorization app
- [`oauth2_third_party_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_third_party_app.md)
  : App representing the third-party app

## git app

- [`git_app()`](https://webfakes.r-lib.org/dev/reference/git_app.md) :
  Web app that acts as a git http server

## Misc Utilities

- [`http_time_stamp()`](https://webfakes.r-lib.org/dev/reference/http_time_stamp.md)
  : Format a time stamp for HTTP
