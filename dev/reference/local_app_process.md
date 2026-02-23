# App process that is cleaned up automatically

You can start the process with an explicit `$start()` call.
Alternatively it starts up at the first `$url()` or `$get_port()` call.

## Usage

``` r
local_app_process(app, ..., .local_envir = parent.frame())
```

## Arguments

- app:

  `webfakes_app` object, the web app to run.

- ...:

  Passed to
  [`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md).

- .local_envir:

  The environment to attach the process cleanup to. Typically a frame.
  When this frame finishes, the process is stopped.

## See also

[`new_app_process()`](https://webfakes.r-lib.org/dev/reference/new_app_process.md)
for more details.
