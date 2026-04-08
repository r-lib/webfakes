# Middleware to parse Cookies

Adds the cookies as the `cookies` element of the request object.

## Usage

``` r
mw_cookie_parser()
```

## Value

Handler function.

## Details

It ignores cookies in an invalid format. It ignores duplicate cookies:
if two cookies have the same name, only the first one is included.

## See also

Other middleware:
[`mw_cgi()`](https://webfakes.r-lib.org/reference/mw_cgi.md),
[`mw_etag()`](https://webfakes.r-lib.org/reference/mw_etag.md),
[`mw_json()`](https://webfakes.r-lib.org/reference/mw_json.md),
[`mw_log()`](https://webfakes.r-lib.org/reference/mw_log.md),
[`mw_multipart()`](https://webfakes.r-lib.org/reference/mw_multipart.md),
[`mw_range_parser()`](https://webfakes.r-lib.org/reference/mw_range_parser.md),
[`mw_raw()`](https://webfakes.r-lib.org/reference/mw_raw.md),
[`mw_static()`](https://webfakes.r-lib.org/reference/mw_static.md),
[`mw_text()`](https://webfakes.r-lib.org/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/reference/mw_urlencoded.md)
