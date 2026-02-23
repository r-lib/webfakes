# Fake OAuth 2.0 resource and authorization app

The webfakes package comes with two fake apps that allow to imitate the
OAuth2.0 flow in your test cases. (See [Aaron Parecki’s
tutorial](https://aaronparecki.com/oauth-2-simplified/) for a good
introduction to OAuth2.0.) One app (`oauth2_resource_app()`) is the API
server that serves both as the resource and provides authorization.
[`oauth2_third_party_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_third_party_app.md)
plays the role of the third-party app. They are useful when testing or
demonstrating code handling OAuth2.0 authorization, token caching, etc.
in a package. The apps can be used in your tests directly, or you could
adapt one or both of them to better mimic a particular OAuth2.0 flow.

## Usage

``` r
oauth2_resource_app(
  access_duration = 3600L,
  refresh_duration = 7200L,
  refresh = TRUE,
  seed = NULL,
  authorize_endpoint = "/authorize",
  token_endpoint = "/token"
)
```

## Arguments

- access_duration:

  After how many seconds should access tokens expire.

- refresh_duration:

  After how many seconds should refresh tokens expire (ignored if
  `refresh` is `FALSE`).

- refresh:

  Should a refresh token be returned (logical).

- seed:

  Random seed used when creating tokens. If `NULL`, we rely on R to
  provide a seed. The app uses its own RNG stream, so it does not affect
  reproducibility of the tests.

- authorize_endpoint:

  The authorization endpoint of the resource server. Change this from
  the default if the real app that you are faking does not use
  `/authorize`.

- token_endpoint:

  The endpoint to request tokens. Change this if the real app that you
  are faking does not use `/token`.

## Value

a `webfakes` app

webfakes app

## Details

The app has the following endpoints:

- `GET /register` is the endpoint that you can use to register your
  third party app. It needs to receive the `name` of the third party
  app, and its `redirect_uri` as query parameters, otherwise returns an
  HTTP 400 error. On success it returns a JSON dictionary with entries
  `name` (the name of the third party app), `client_id`, `client_secret`
  and `redirect_uri`.

- `GET /authorize` is the endpoint where the user of the third party app
  is sent. You can change the URL of this endpoint with the
  `authorize_endpoint` argument. It needs to receive the `client_id` of
  the third party app, and its correct `redirect_uri` as query
  parameters. It may receive a `state` string as well, which can be used
  by a client to identify the request. Otherwise it generates a random
  `state` string. On error it fails with a HTTP 400 error. On success it
  returns a simple HTML login page.

- `POST /authorize/decision` is the endpoint where the HTML login page
  generated at `/authorize` connects back to, either with a positive or
  negative result. The form on the login page will send the `state`
  string and the user's choice in the `action` variable. If the user
  authorized the third party app, then they are redirected to the
  `redirect_uri` of the app, with a temporary `code` and the `state`
  string supplied as query parameters. Otherwise a simple HTML page is
  returned.

- `POST /token` is the endpoint where the third party app requests a
  temporary access token. It is also uses for refreshing an access token
  with a refresh token. You can change the URL of this endpoint with the
  `token_endpoint` argument. To request a new token or refresh an
  existing one, the following data must be included in either a JSON or
  an URL encoded request body:

  - `grant_type`, this must be `authorization_code` for new tokens, and
    `refresh_token` for refreshing.

  - `code`, this must be the temporary code obtained from the
    `/authorize/decision` redirection, for new tokens. It is not needed
    when refreshing.

  - `client_id` must be the client id of the third party app.

  - `client_secret` must be the client secret of the third party app.

  - `redirect_uri` must be the correct redirection URI of the third
    party app. It is not needed when refreshing tokens.

  - `refresh_token` must be the refresh token obtained previously, when
    refreshing a token. It is not needed for new tokens. On success a
    JSON dictionary is returned with entries: `access_token`, `expiry`
    and `refresh_token`. (The latter is omitted if the `refresh`
    argument is `FALSE`).

- `GET /locals` returns a list of current apps, access tokens and
  refresh tokens.

- `GET /data` is an endpoint that returns a simple JSON response, and
  needs authorization.

### Notes

- Using this app in your tests requires the glue package, so you need to
  put it in `Suggests`.

- You can add custom endpoints to the app, as needed.

- If you need authorization in your custom endpoint, call
  `app$is_authorized()` in your handler:

      if (!app$is_authorized(req, res)) return()

  `app$is_authorized()` returns an HTTP 401 response if the client is
  not authorized, so you can simply return from your handler.

For more details see
[`vignette("oauth", package = "webfakes")`](https://webfakes.r-lib.org/dev/articles/oauth.md).

## `oauth2_resource_app()`

App representing the API server (resource/authorization)

## See also

Other OAuth2.0 functions:
[`oauth2_httr_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_httr_login.md),
[`oauth2_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_login.md),
[`oauth2_third_party_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_third_party_app.md)
