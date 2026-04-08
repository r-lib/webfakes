# Create a new regular expression to use in webfakes routes

Note that webfakes uses PERL regular expressions.

## Usage

``` r
new_regexp(x)
```

## Arguments

- x:

  String scalar containing a regular expression.

## Value

String with class `webfakes_regexp`.

## Details

As R does not have data type or class for regular expressions, you can
use `new_regexp()` to mark a string as a regular expression, when adding
routes.

## See also

The 'Path specification' and 'Path parameters' chapters of the manual of
[`new_app()`](https://webfakes.r-lib.org/reference/new_app.md).

## Examples

``` r
new_regexp("^/api/match/(?<pattern>.*)$")
#> <webfakes_regexp> "^/api/match/(?<pattern>.*)$"
```
