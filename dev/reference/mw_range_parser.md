# Middleware to parse a Range header

Adds the requested ranges to the `ranges` element of the request object.
`request$ranges` is a data frame with two columns, `from` and `to`. Each
row corresponds one requested interval.

## Usage

``` r
mw_range_parser()
```

## Value

Handler function.

## Details

When the last `n` bytes of the file are requested, the matrix row is set
to `c(0, -n)`. When all bytes after a `p` position are requested, the
matrix row is set to `c(p, Inf)`.

If the intervals overlap, then `ranges` is not set, i.e. the `Range`
header is ignored.

If its syntax is invalid or the unit is not `bytes`, then the `Range`
header is ignored.

## See also

Other middleware:
[`mw_cgi()`](https://webfakes.r-lib.org/dev/reference/mw_cgi.md),
[`mw_cookie_parser()`](https://webfakes.r-lib.org/dev/reference/mw_cookie_parser.md),
[`mw_etag()`](https://webfakes.r-lib.org/dev/reference/mw_etag.md),
[`mw_json()`](https://webfakes.r-lib.org/dev/reference/mw_json.md),
[`mw_log()`](https://webfakes.r-lib.org/dev/reference/mw_log.md),
[`mw_multipart()`](https://webfakes.r-lib.org/dev/reference/mw_multipart.md),
[`mw_raw()`](https://webfakes.r-lib.org/dev/reference/mw_raw.md),
[`mw_static()`](https://webfakes.r-lib.org/dev/reference/mw_static.md),
[`mw_text()`](https://webfakes.r-lib.org/dev/reference/mw_text.md),
[`mw_urlencoded()`](https://webfakes.r-lib.org/dev/reference/mw_urlencoded.md)
