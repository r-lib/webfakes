# Helper function to use httr's OAuth2.0 functions non-interactively, e.g. in test cases

To perform an automatic acknowledgement and log in for a local OAuth2.0
app, run by httr, wrap the expression that obtains the OAuth2.0 token in
`oauth2_httr_login()`.

## Usage

``` r
oauth2_httr_login(expr)
```

## Arguments

- expr:

  Expression that calls
  [`httr::oauth2.0_token()`](https://httr.r-lib.org/reference/oauth2.0_token.html),
  either directly, or indirectly.

## Value

The return value of `expr`.

## Details

In interactive sessions, `oauth2_httr_login()` overrides the `browser`
option, and when httr opens a browser page, it calls
[`oauth2_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_login.md)
in a subprocess.

In non-interactive sessions, httr does not open a browser page, only
messages the user to do it manually. `oauth2_httr_login()` listens for
these messages, and calls
[`oauth2_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_login.md)
in a subprocess.

## See also

See `?vignette("oauth", package = "webfakes")` for a case study that
uses this function.

Other OAuth2.0 functions:
[`oauth2_login()`](https://webfakes.r-lib.org/dev/reference/oauth2_login.md),
[`oauth2_resource_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_resource_app.md),
[`oauth2_third_party_app()`](https://webfakes.r-lib.org/dev/reference/oauth2_third_party_app.md)
