# glue based template engine

Use this template engine to create pages with glue templates. See
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for the
syntax.

## Usage

``` r
tmpl_glue(
  sep = "",
  open = "{",
  close = "}",
  na = "NA",
  transformer = NULL,
  trim = TRUE
)
```

## Arguments

- sep:

  Separator used to separate elements.

- open:

  The opening delimiter. Doubling the full delimiter escapes it.

- close:

  The closing delimiter. Doubling the full delimiter escapes it.

- na:

  Value to replace NA values with. If `NULL` missing values are
  propagated, that is an `NA` result will cause `NA` output. Otherwise
  the value is replaced by the value of `na`.

- transformer:

  A function taking three parameters `code`, `envir` and `data` used to
  transform the output of each block before during or after evaluation.

- trim:

  Whether to trim the input template with
  [`glue::trim()`](https://glue.tidyverse.org/reference/trim.html) or
  not.

## Value

Template function.

## Examples

``` r
# See th 'hello' app at
hello_root <- system.file(package = "webfakes", "examples", "hello")
hello_root
#> [1] "/home/runner/work/_temp/Library/webfakes/examples/hello"

app <- new_app()
app$engine("txt", tmpl_glue())
app$use(mw_log())


app$get("/view", function(req, res) {
  txt <- res$render("test")
  res$
    set_type("text/plain")$
    send(txt)
})

# Switch to the app's root: setwd(hello_root)
# Now start the app with: app$listen(3000L)
# Or start it in another process: new_process(app)
```
