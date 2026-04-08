# App representing the third-party app

The webfakes package comes with two fake apps that allow to imitate the
OAuth2.0 flow in your test cases. (See [Aaron Parecki’s
tutorial](https://aaronparecki.com/oauth-2-simplified/) for a good
introduction to OAuth2.0.) One app
([`oauth2_resource_app()`](https://webfakes.r-lib.org/reference/oauth2_resource_app.md))
is the API server that serves both as the resource and provides
authorization. `oauth2_third_party_app()` plays the role of the
third-party app. They are useful when testing or demonstrating code
handling OAuth2.0 authorization, token caching, etc. in a package. The
apps can be used in your tests directly, or you could adapt one or both
of them to better mimic a particular OAuth2.0 flow.

## Usage

``` r
oauth2_third_party_app(name = "Third-Party app")
```

## Arguments

- name:

  Name of the third-party app

## Value

webfakes app

## Details

Endpoints:

- `POST /login/config` Use this endpoint to configure the client ID and
  the client secret of the app, received from
  [`oauth2_resource_app()`](https://webfakes.r-lib.org/reference/oauth2_resource_app.md)
  (or another resource app). You need to send in a JSON or URL encoded
  body:

  - `auth_url`, the authorization URL of the resource app.

  - `token_url`, the token URL of the resource app.

  - `client_id`, the client ID, received from the resource app.

  - `client_secret` the client secret, received from the resource app.

- `GET /login` Use this endpoint to start the login process. It will
  redirect to the resource app for authorization and after the OAuth2.0
  dance to `/login/redirect`.

- `GET /login/redirect`, `POST /login/redirect` This is the redirect URI
  of the third party app. (Some HTTP clients redirect a `POST` to a
  `GET`, others don't, so it has both.) This endpoint is used by the
  resource app, and it received the `code` that can be exchanged to an
  access token and the `state` which was generated in `/login`. It
  contacts the resource app to get an access token, and then stores the
  token in its `app$locals` local variables. It fails with HTTP code 500
  if it cannot obtain an access token. On success it returns a JSON
  dictionary with `access_token`, `expiry` and `refresh_token`
  (optionally) by default. This behavior can be changed by redefining
  the `app$redirect_hook()` function.

- `GET /locals` returns the tokens that were obtained from the resource
  app.

- `GET /data` is an endpoint that uses the obtained token(s) to connect
  to the `/data` endpoint of the resource app. The `/data` endpoint of
  the resource app needs authorization. It responds with the response of
  the resource app. It tries to refresh the access token of the app if
  needed.

For more details see
[`vignette("oauth", package = "webfakes")`](https://webfakes.r-lib.org/articles/oauth.md).

## See also

Other OAuth2.0 functions:
[`oauth2_httr_login()`](https://webfakes.r-lib.org/reference/oauth2_httr_login.md),
[`oauth2_login()`](https://webfakes.r-lib.org/reference/oauth2_login.md),
[`oauth2_resource_app()`](https://webfakes.r-lib.org/reference/oauth2_resource_app.md)
