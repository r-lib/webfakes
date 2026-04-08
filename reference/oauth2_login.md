# Helper function to log in to a third party OAuth2.0 app without a browser

It works with
[`oauth2_resource_app()`](https://webfakes.r-lib.org/reference/oauth2_resource_app.md),
and any third party app, including the fake
[`oauth2_third_party_app()`](https://webfakes.r-lib.org/reference/oauth2_third_party_app.md).

## Usage

``` r
oauth2_login(login_url)
```

## Arguments

- login_url:

  The login URL of the third party app.

## Value

A named list with

- `login_response` The curl HTTP response object for the login page.

- `token_response` The curl HTTP response object for submitting the
  login page.

## Details

See `test-oauth.R` in webfakes for an example.

## See also

Other OAuth2.0 functions:
[`oauth2_httr_login()`](https://webfakes.r-lib.org/reference/oauth2_httr_login.md),
[`oauth2_resource_app()`](https://webfakes.r-lib.org/reference/oauth2_resource_app.md),
[`oauth2_third_party_app()`](https://webfakes.r-lib.org/reference/oauth2_third_party_app.md)
