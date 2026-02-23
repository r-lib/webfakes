# Web app that acts as a git http server

It is useful for tests that need an HTTP git server.

## Usage

``` r
git_app(
  git_root,
  git_cmd = "git",
  git_timeout = as.difftime(1, units = "mins"),
  filter = TRUE,
  cleanup = TRUE
)
```

## Arguments

- git_root:

  Path to the root of the directory tree to be served.

- git_cmd:

  Command to call, by default it is `"git"`. It may also be a full path
  to git.

- git_timeout:

  A `difftime` object, time limit for the git command.

- filter:

  Whether to support the `filter` capability in the server.

- cleanup:

  Whether to clean up `git_root` when the app is garbage collected.

## Examples

``` r
if (FALSE) {
dir.create(tmp <- tempfile())
setwd(tmp)
system("git clone --bare https://github.com/cran/crayon")
system("git clone --bare https://github.com/cran/glue")
app <- git_app(tmp)
git <- new_app_process(app)
system(paste("git ls-remote", git$url("/crayon")))
}
```
