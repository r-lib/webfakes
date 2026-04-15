# OAuth2.0 webfakes apps

``` r
library(webfakes)
```

The webfakes package comes with two fake apps that allow to imitate the
OAuth2.0 flow in your test cases. (See [Aaron Parecki’s
tutorial](https://aaronparecki.com/oauth-2-simplified/) for a good
introduction to OAuth2.0.) One app
([`oauth2_resource_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_resource_app.md))
is the API server that serves both as the resource and provides
authorization.
[`oauth2_third_party_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_third_party_app.md)
plays the role of the third-party app. They are useful when testing or
demonstrating code handling OAuth2.0 authorization, token caching, etc.
in a package. The apps can be used in your tests directly, or you could
adapt one or both of them to better mimic a particular OAuth2.0 flow.

In this vignette, we shall look at how to implement a flow using both
apps, or the server app together with httr’s OAuth tools. In both cases
we shall use the httr package as HTTP client. We shall end with a case
study for OAuth2.0 testing. For an example using the curl package
instead, look at the test file `test-oauth.R` of webfakes instead.

## The OAuth2.0 resource and authorization server

First we need to create the resource server, which also performs the
authorization, and we create variables holding its different URLs.

``` r
templog <- tempfile()
rsapp <- new_app_process(
  oauth2_resource_app(
    refresh_duration = .Machine$integer.max,
    access_duration = 10L,
  ),
  opts = server_opts(num_threads = 3)
)

regi_url <- rsapp$url("/register")
auth_url <- rsapp$url("/authorize")
toke_url <- rsapp$url("/token")

rsapp
#> <webfakes_app_process>
#> state:
#>   live
#> auto_start:
#>   TRUE
#> process id:
#>   8940
#> http url:
#>   http://127.0.0.1:40301/
#> fields and methods:
#>   get_app()              # get the app object
#>   get_port()             # query (first) port of the app
#>   get_ports()            # query all ports of the app
#>   get_state()            # query web server process state
#>   local_env(envvars)     # set temporary environment variables
#>   start()                # start the app
#>   url(path, query)       # query url for an api path
#>   stop()                 # stop web server process
#> # see ?webfakes_app_process for details
```

## Fake third party application

### OAuth2.0 app creation & registration

Then we create the third-party app, and we create variables holding its
different URLs.

``` r
tpapp <- new_app_process(
  oauth2_third_party_app("3P app"),
  opts = server_opts(num_threads = 3)
)

redi_url <- tpapp$url("/login/redirect")
conf_url <- tpapp$url("/login/config")

tpapp
#> <webfakes_app_process>
#> state:
#>   live
#> auto_start:
#>   TRUE
#> process id:
#>   8953
#> http url:
#>   http://127.0.0.1:33629/
#> fields and methods:
#>   get_app()              # get the app object
#>   get_port()             # query (first) port of the app
#>   get_ports()            # query all ports of the app
#>   get_state()            # query web server process state
#>   local_env(envvars)     # set temporary environment variables
#>   start()                # start the app
#>   url(path, query)       # query url for an api path
#>   stop()                 # stop web server process
#> # see ?webfakes_app_process for details
```

We then need to register the third-party app at the resource server. In
real life this is done by the admin of the third party app. Our fake
resource server provides an endpoint at `/register` (in `regi_url`) to
do this automatically, without user interaction. We need to send the
name of our third party app, and its redirect URL, as query parameters.

``` r
url <- paste0(
  regi_url,
  "?name=3P%20app",
  "&redirect_uri=", redi_url
)
reg_resp <- httr::GET(url)
reg_resp
#> Response [http://127.0.0.1:40301/register?name=3P%20app&redirect_uri=http://127.0.0.1:33629/login/redirect]
#>   Date: 2026-04-15 07:25
#>   Status: 200
#>   Content-Type: application/json
#>   Size: 184 B
regdata <- httr::content(reg_resp)
regdata
#> $name
#> $name[[1]]
#> [1] "3P app"
#> 
#> 
#> $client_id
#> $client_id[[1]]
#> [1] "id-5ec26ea76cb0e1200c2248fe1f4a06"
#> 
#> 
#> $client_secret
#> $client_secret[[1]]
#> [1] "secret-e76a07d14f3f17e2e91b72787496e0"
#> 
#> 
#> $redirect_uri
#> $redirect_uri[[1]]
#> [1] "http://127.0.0.1:33629/login/redirect"
```

The resource app replies with the client id and the client secret. We’ll
use them to authenticate the third party app. In real life they are
included in the config of the third party app by its admin. Our third
party app has an API endpoint, `/login/config` (already in `conf_url`)
to configure them.

``` r
auth_data <- list(
  auth_url = auth_url,
  token_url = toke_url,
  client_id = regdata$client_id[[1]],
  client_secret = regdata$client_secret[[1]]
)

httr::POST(
  conf_url,
  body = auth_data,
  encode = "json"
)
#> Response [http://127.0.0.1:33629/login/config]
#>   Date: 2026-04-15 07:25
#>   Status: 200
#>   Content-Type: application/json
#>   Size: 41 B
```

### The OAuth2.0 dance

Now a user can go to the login URL of the third party app, `/login` in
our fake app, to authenticate. To start the web page from R, you can run

``` r
browseURL(tpapp$url("/login"))
```

The third party app now has a token, that it can use to authenticate to
the resource app. See the `test-oauth.R` file within webfakes to see how
to do this part programmatically, without a browser.

By default our fake third party app saves the token(s) into a local
variable, and also returns in JSON, so you can see that in the browser:

    #> {
    #>   "access_token": "token-c6be45eee35844e7ec1d6ada44bc15",
    #>   "expiry": 10,
    #>   "refresh_token": "refresh-token-ee3f1285a6f4585e9f410375e0512d"
    #> }

If you want to change this behavior, you can define the `redirect_hook`
function in the third party app. For example:

``` r
thirdp <- oauth2_third_party_app("3P app")
thirdp$redirect_hook <- function(res, tokens) {
  res$
    set_status(200L)$
    send("Authentication complete, return to R!")
}

tpapp2 <- new_app_process(
  thirdp,
  opts = server_opts(num_threads = 3)
)

redi_url2 <- tpapp2$url("/login/redirect")
conf_url2 <- tpapp2$url("/login/config")

tpapp2
#> <webfakes_app_process>
#> state:
#>   live
#> auto_start:
#>   TRUE
#> process id:
#>   8967
#> http url:
#>   http://127.0.0.1:35695/
#> fields and methods:
#>   get_app()              # get the app object
#>   get_port()             # query (first) port of the app
#>   get_ports()            # query all ports of the app
#>   get_state()            # query web server process state
#>   local_env(envvars)     # set temporary environment variables
#>   start()                # start the app
#>   url(path, query)       # query url for an api path
#>   stop()                 # stop web server process
#> # see ?webfakes_app_process for details

url2 <- paste0(
  regi_url,
  "?name=3P%20app2",
  "&redirect_uri=", redi_url2
)
reg_resp2 <- httr::GET(url2)
reg_resp2
#> Response [http://127.0.0.1:40301/register?name=3P%20app2&redirect_uri=http://127.0.0.1:35695/login/redirect]
#>   Date: 2026-04-15 07:25
#>   Status: 200
#>   Content-Type: application/json
#>   Size: 185 B
regdata2 <- httr::content(reg_resp2)
regdata2
#> $name
#> $name[[1]]
#> [1] "3P app2"
#> 
#> 
#> $client_id
#> $client_id[[1]]
#> [1] "id-8c4c1b4e872b03c419c8ea3980ab6e"
#> 
#> 
#> $client_secret
#> $client_secret[[1]]
#> [1] "secret-f333d435f1839309c3ac6af907db12"
#> 
#> 
#> $redirect_uri
#> $redirect_uri[[1]]
#> [1] "http://127.0.0.1:35695/login/redirect"
auth_data2 <- list(
  auth_url = auth_url,
  token_url = toke_url,
  client_id = regdata2$client_id[[1]],
  client_secret = regdata2$client_secret[[1]]
)

httr::POST(
  conf_url2,
  body = auth_data2,
  encode = "json"
)
#> Response [http://127.0.0.1:35695/login/config]
#>   Date: 2026-04-15 07:25
#>   Status: 200
#>   Content-Type: application/json
#>   Size: 41 B
```

Then again you can authenticate at the new app with

``` r
browseURL(tpapp2$url("/login"))
```

    #> Authentication complete, return to R!

The fake third party app also has an endpoint to return the saved
tokens:

``` r
httr::content(httr::GET(tpapp2$url("/locals")))
#> $access_token
#> [1] "token-08e1470fb2bbfa9216925390655281"
#> 
#> $expiry
#> [1] 10
#> 
#> $refresh_token
#> [1] "refresh-token-f70b06b589156b9b5d462b040c500c"
```

Now the third-party app can get data on your behalf (the whole goal of
OAuth!) — it could also post data on your behalf if the resource app had
endpoints for that.

For example the `/data` endpoint of the third party app queries the
resource app and needs authentication. If you run the following without
the OAuth dance, your access is denied. But now it works fine:

``` r
resp_data <- httr::GET(tpapp2$url("/data"))
resp_data
#> Response [http://127.0.0.1:35695/data]
#>   Date: 2026-04-15 07:25
#>   Status: 200
#>   Content-Type: application/json
#>   Size: 24 B
httr::content(resp_data, as = "text")
#> No encoding supplied: defaulting to UTF-8.
#> [1] "{\"data\":[\"top secret!\"]}"
```

In real life, access by the third-party app might be limited to some
scopes, but the fake apps shipped with webfakes do not handle that.

## The fake resource server and httr

When you use httr’s OAuth tool, there’s some gymnastics happening as R
is playing the role of a third-party app via httr and httpuv (to listen
to the redirect URI).

### OAuth2.0 app creation & registration

What’s crucial here is setting
[`httr::oauth_callback()`](https://httr.r-lib.org/reference/oauth_callback.html)
as redirect URI, which is what you do when creating an app for an R
package that uses OAuth2.0 to authenticate to a resource server.

``` r
url3 <- paste0(
  regi_url,
  "?name=3P%20app2",
  "&redirect_uri=", httr::oauth_callback()
)
reg_resp3 <- httr::GET(url3)
reg_resp3
#> Response [http://127.0.0.1:40301/register?name=3P%20app2&redirect_uri=http://localhost:1410/]
#>   Date: 2026-04-15 07:25
#>   Status: 200
#>   Content-Type: application/json
#>   Size: 170 B
regdata3 <- httr::content(reg_resp3)
regdata3
#> $name
#> $name[[1]]
#> [1] "3P app2"
#> 
#> 
#> $client_id
#> $client_id[[1]]
#> [1] "id-e77a784da2d515ba369eb98c6bb44a"
#> 
#> 
#> $client_secret
#> $client_secret[[1]]
#> [1] "secret-67ff92fb586bd54d7d0f8b446f9653"
#> 
#> 
#> $redirect_uri
#> $redirect_uri[[1]]
#> [1] "http://localhost:1410/"
```

Now we set the registration data on the third-party app.

``` r
app <- httr::oauth_app(
  "3P app2",
  key = regdata3$client_id[[1]],
  secret = regdata3$client_secret[[1]],
  redirect_uri = httr::oauth_callback()
)

endpoint <- httr::oauth_endpoint(
  authorize = auth_url,
  access = toke_url
)
```

Now we can launch the token creation.

``` r
token <- oauth2_httr_login(
  httr::oauth2.0_token(endpoint, app, cache = FALSE)
)
#> Waiting for authentication in browser...
#> Press Esc/Ctrl + C to abort
#> Authentication complete.
```

``` r
token
#> <Token>
#> <oauth_endpoint>
#>  authorize: http://127.0.0.1:40301/authorize
#>  access:    http://127.0.0.1:40301/token
#> <oauth_app> 3P app2
#>   key:    id-e77a784da2d515ba369eb98c6bb44a
#>   secret: <hidden>
#> <credentials> access_token, expiry, refresh_token
#> ---
```

Without the token, the query to the resource server fails:

``` r
httr::GET(rsapp$url("/data"))
#> Response [http://127.0.0.1:40301/data]
#>   Date: 2026-04-15 07:25
#>   Status: 401
#>   Content-Type: text/plain
#>   Size: 20 B
```

With the token, it is successful. httr also automatically refreshes the
token if needed.

``` r
httr::content(
  httr::GET(rsapp$url("/data"), config = token),
  as = "text"
)
#> No encoding supplied: defaulting to UTF-8.
#> [1] "{\"data\":[\"top secret!\"]}"
```

## Advanced topics

### Applications

With these apps, or only the resource server app, you can now test your
code that helps users create and store an OAuth2.0 token.

Like all webfakes apps, OAuth2.0 apps are extensible: you can add your
own endpoints and middleware to it. E.g. here we add logging via
[`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md) and a
new endpoint.

``` r
rsapp2 <- oauth2_resource_app(
  refresh_duration = .Machine$integer.max,
  access_duration = 10L
)
logfile <- tempfile("oauth-rs-", fileext = ".log")
rsapp2$use(mw_log(stream = logfile), .first = TRUE)
rsapp2$get("/docs", function(req, res) {
  res$
    set_status(200L)$
    send("See vignette('oauth', package = 'webfakes')")
})

rsapp2_process <- new_app_process(
  rsapp2,
  opts = server_opts(num_threads = 3)
)
```

If you want to customize one of the apps or both apps a lot, it might
make sense to use their source code as starting point or inspiration.

### Debugging

See the [usual debugging advice for webfakes
apps](https://r-lib.github.io/webfakes/dev/articles/how-to.html#how-can-i-debug-an-app-).
In particular, you can add the
[`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md)
middleware to write the log of the app to a file, like we did above.

The resource app has a `/locals` endpoint, that returns all data stored
in the app, this includes the tokens and the refresh tokens:

``` r
httr::content(
  httr::GET(rsapp$url("/locals"))
)
#> $apps
#> $apps[[1]]
#> $apps[[1]]$name
#> [1] "3P app"
#> 
#> $apps[[1]]$client_id
#> [1] "id-5ec26ea76cb0e1200c2248fe1f4a06"
#> 
#> $apps[[1]]$client_secret
#> [1] "secret-e76a07d14f3f17e2e91b72787496e0"
#> 
#> $apps[[1]]$redirect_uri
#> [1] "http://127.0.0.1:33629/login/redirect"
#> 
#> 
#> $apps[[2]]
#> $apps[[2]]$name
#> [1] "3P app2"
#> 
#> $apps[[2]]$client_id
#> [1] "id-8c4c1b4e872b03c419c8ea3980ab6e"
#> 
#> $apps[[2]]$client_secret
#> [1] "secret-f333d435f1839309c3ac6af907db12"
#> 
#> $apps[[2]]$redirect_uri
#> [1] "http://127.0.0.1:35695/login/redirect"
#> 
#> 
#> $apps[[3]]
#> $apps[[3]]$name
#> [1] "3P app2"
#> 
#> $apps[[3]]$client_id
#> [1] "id-e77a784da2d515ba369eb98c6bb44a"
#> 
#> $apps[[3]]$client_secret
#> [1] "secret-67ff92fb586bd54d7d0f8b446f9653"
#> 
#> $apps[[3]]$redirect_uri
#> [1] "http://localhost:1410/"
#> 
#> 
#> 
#> $access
#> $access[[1]]
#> $access[[1]]$client_id
#> [1] "id-5ec26ea76cb0e1200c2248fe1f4a06"
#> 
#> $access[[1]]$token
#> [1] "token-c6be45eee35844e7ec1d6ada44bc15"
#> 
#> $access[[1]]$expiry
#> [1] "2026-04-15 07:25:45"
#> 
#> 
#> $access[[2]]
#> $access[[2]]$client_id
#> [1] "id-8c4c1b4e872b03c419c8ea3980ab6e"
#> 
#> $access[[2]]$token
#> [1] "token-08e1470fb2bbfa9216925390655281"
#> 
#> $access[[2]]$expiry
#> [1] "2026-04-15 07:25:45"
#> 
#> 
#> $access[[3]]
#> $access[[3]]$client_id
#> [1] "id-e77a784da2d515ba369eb98c6bb44a"
#> 
#> $access[[3]]$token
#> [1] "token-1f46a0366717828ac5cc842c163a31"
#> 
#> $access[[3]]$expiry
#> [1] "2026-04-15 07:25:46"
#> 
#> 
#> 
#> $refresh
#> $refresh[[1]]
#> $refresh[[1]]$client_id
#> [1] "id-5ec26ea76cb0e1200c2248fe1f4a06"
#> 
#> $refresh[[1]]$token
#> [1] "refresh-token-ee3f1285a6f4585e9f410375e0512d"
#> 
#> $refresh[[1]]$expiry
#> [1] "2094-05-03 10:39:42"
#> 
#> 
#> $refresh[[2]]
#> $refresh[[2]]$client_id
#> [1] "id-8c4c1b4e872b03c419c8ea3980ab6e"
#> 
#> $refresh[[2]]$token
#> [1] "refresh-token-f70b06b589156b9b5d462b040c500c"
#> 
#> $refresh[[2]]$expiry
#> [1] "2094-05-03 10:39:42"
#> 
#> 
#> $refresh[[3]]
#> $refresh[[3]]$client_id
#> [1] "id-e77a784da2d515ba369eb98c6bb44a"
#> 
#> $refresh[[3]]$token
#> [1] "refresh-token-81d2b2f09bcf64302605ab6a9750b3"
#> 
#> $refresh[[3]]$expiry
#> [1] "2094-05-03 10:39:43"
```

## Case study for OAuth2.0 testing

Consider a package that has a function that uses OAuth2.0 to access
GitHub. In this section we’ll show how you can use webfakes to test this
function.

``` r
gh_base <- function() {
  Sys.getenv("FAKE_GH_API_BASE", "https://api.github.com")
}
gh_oauth_base <- function() {
  Sys.getenv(
    "FAKE_GH_AUTH_BASE",
    "https://github.com/login/oauth"
  )
}
gh_repos <- function() {
  ghapp <- httr::oauth_app(
    "gh_repos",
    key = Sys.getenv("GH_CLIENT_ID"),
    secret = Sys.getenv("GH_CLIENT_SECRET")
  )
  endpoints <- httr::oauth_endpoint(
    base_url = gh_oauth_base(),
    request = NULL,
    authorize = "authorize",
    access = "access_token"
  )
  gh_token <- httr::oauth2.0_token(
    endpoints,
    ghapp,
    cache = FALSE
  )
  httr_token <- httr::config(token = gh_token)
  response <- httr::GET(
    paste0(gh_base(), "/user/repos?visibility=public"),
    httr::add_headers(Accept = "application/vnd.github.v3+json"),
    config = httr_token
  )
  httr::stop_for_status(response)
  json <- httr::content(response, as = "text")
  repos <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  vapply(repos, function(r) r$full_name, character(1))
}
```

`gh_repos()` uses an OAuth2.0 app to get a token from GitHub, and then
uses that token to authenticate and list the public repositories of the
current user. In a real package you would probably get the token in a
separate function, cache it, and re-use it for multiple queries. (Also,
you don’t actually need authorization for listing public repositories,
so this is a somewhat artificial example.)

To run this function, you would need to register an app at
<https://github.com/settings/developers>. Make sure that you set
`http://localhost:1410` as the authorization callback URL. You can set
the other options as you please. Then in R set the `GH_CLIENT_ID` and
`GH_CLIENT_SECRET` environment variables to the client id and client
secret of the app:

``` r
Sys.setenv(GH_CLIENT_ID = "<your client id here>")
Sys.setenv(GH_CLIENT_SECRET = "<your client secret here>")
```

When you run this function it opens a Window in your browser, where you
can authorize the app to access your public information on GitHub. For
the subsequent runs that browser window still opens, but the
authorization is automatic. In a real package you could cache the
OAuth2.0 tokens on the machine, e.g. using the keyring package.

``` r
gh_repos()
```

    #> Waiting for authentication in browser...
    #> Press Esc/Ctrl + C to abort
    #> Authentication complete.
    #>  [1] "gaborcsardi/altlist"     "gaborcsardi/argufy"     
    #>  [3] "gaborcsardi/async"       "gaborcsardi/disposables"
    #>  [5] "gaborcsardi/dotenv"      "gaborcsardi/falsy"      
    #>  [7] "gaborcsardi/franc"       "gaborcsardi/ISA"        
    #>  [9] "gaborcsardi/keypress"    "gaborcsardi/lpSolve"    
    #> [11] "gaborcsardi/macBriain"   "gaborcsardi/maxygen"    
    #> [13] "gaborcsardi/MISO"        "gaborcsardi/msgtools"   
    #> [15] "gaborcsardi/notifier"    "gaborcsardi/odbc"       
    #> [17] "gaborcsardi/parr"        "gaborcsardi/parsedate"  
    #> [19] "gaborcsardi/prompt"      "gaborcsardi/r-font"     
    #> [21] "gaborcsardi/r-source"    "gaborcsardi/rcorpora"   
    #> [23] "gaborcsardi/roxygenlabs" "gaborcsardi/sankey"     
    #> [25] "gaborcsardi/secret"      "gaborcsardi/spark"      
    #> [27] "gaborcsardi/standalones"

Let’s write a test case now for `gh_repos()`. We will use
`oauth2_repource_app()` to fake GitHub.

``` r
testthat::test_that("gh_repos", {
  testthat::skip_on_cran()
  fake_app <- oauth2_resource_app(token_endpoint = "/access_token")
  fake_app$get("/user/repos", function(req, res) {
    if (!app$is_authorized(req, res)) return()
    res$send_json(list(
      list(full_name = "user/repo1"),
      list(full_name = "user/repo2")
    ), auto_unbox = TRUE)
  })

  fake_proc <- local_app_process(
    fake_app,
    opts = server_opts(num_threads = 3)
  )

  # register the app to our fake GH server
  reg_url <- paste0(
    fake_proc$url("/register"),
    "?name=gh_repos&redirect_uri=http://localhost:1410/"
  )
  regdata <- httr::content(httr::GET(reg_url))
  withr::local_envvar(
    GH_CLIENT_ID = regdata$client_id[[1]],
    GH_CLIENT_SECRET = regdata$client_secret[[1]],
    FAKE_GH_API_BASE = fake_proc$url(),
    FAKE_GH_AUTH_BASE = fake_proc$url()
  )

  ret <- suppressMessages(webfakes::oauth2_httr_login(
    gh_repos()
  ))
  testthat::expect_equal(ret, c("user/repo1", "user/repo2"))
})
#> Test passed with 1 success 🎉.
```

Some important points about this test case:

- Since the test case will fail if port 1410 is taken, it is safer to
  skip it on CRAN.

- `fake_app` is a webfakes app, that is almost the same as
  [`oauth2_resource_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_resource_app.md),
  with two changes.

  - The first is the we change the end point for getting a token to
    `/access_token` because that is what GitHub uses.
  - The second is that we also add the `/user/repos` endpoint. This
    endpoint needs authorization, this is why it calls
    `app$is_authorized()`, which fails without it, and the app returns
    401 Access denied.

- `fake_proc` is the app process that runs our fake GH app. Always run
  the fake OAuth2.0 apps with multiple threads.

- Our fake GitHub app has an endpoint to register the app we are
  testing. We need to send the app’s name and the correct redirect URI,
  and `fake_app` will reply with a fake client id and client secret.

- We set `GH_CLIENT_ID` and `GH_CLIENT_SECRET` to the fake ones we just
  got from `fake_app`.

- We also redirect `gh_token()` to the fake app, by setting
  `FAKE_GH_API_BASE` and `FAKE_GH_AUTH_BASE`.

- Now if we call `gh_repos()`, it will connect to our fake app. We also
  need to make sure that httr can log in to the fake app, without a
  browser interaction. The
  [`webfakes::oauth2_httr_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_httr_login.md)
  wrapper takes care of that. It runs an HTTP client in a background
  process, to perform the log in.

- If the background process in
  [`webfakes::oauth2_httr_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_httr_login.md)
  fails to log in for some reason, the test code might freeze. This is
  another good reason to skip this test on CRAN.

- [`suppressMessages()`](https://rdrr.io/r/base/message.html) suppresses
  the (harmless) httr messages about the authorization.

- The test case does have a lot of boilerplate to set up and manage the
  fake apps. Note that you can refactor this code into its own helper
  function, that starts up the fake app sub-process on demand in a
  helper file. That way you can reuse it for multiple test files and
  test cases.

This test case ensures that the OAuth2.0 setup in `gh_repos()` stays
correct.
