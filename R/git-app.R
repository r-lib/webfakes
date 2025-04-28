#' Web app that acts as a git http server
#'
#' It is useful for tests that need an HTTP git server.
#'
#' @param git_root Path to the root of the directory tree to be served.
#' @param git_cmd Command to call, by default it is `"git"`. It may also
#'   be a full path to git.
#' @param git_timeout A `difftime` object, time limit for the git
#'   command.
#' @param filter Whether to support the `filter` capability in the server.
#' @param cleanup Whether to clean up `git_root` when the app is
#'   garbage collected.
#'
#' @export
#' @examplesIf FALSE
#' dir.create(tmp <- tempfile())
#' setwd(tmp)
#' system("git clone --bare https://github.com/cran/crayon")
#' system("git clone --bare https://github.com/cran/glue")
#' app <- git_app(tmp)
#' git <- new_app_process(app)
#' system(paste("git ls-remote", git$url("/crayon")))

git_app <- function(
  git_root,
  git_cmd = "git",
  git_timeout = as.difftime(1, units = "mins"),
  filter = TRUE,
  cleanup = TRUE
) {
  app <- webfakes::new_app()
  app$locals$git_root <- git_root
  app$locals$git_timeout <- as.double(git_timeout, units = "secs") * 1000
  app$locals$git_config <- tempfile()

  reg.finalizer(app, function(app0) unlink(app$locals$git_config), TRUE)
  writeLines(
    c(
      "[uploadpack]",
      paste0("\tallowFilter = ", if (isTRUE(filter)) "true" else "false")
    ),
    app$locals$git_config
  )

  if (cleanup) {
    reg.finalizer(
      app,
      function(app) unlink(app$locals$git_root, recursive = TRUE),
      TRUE
    )
  }

  cgi <- mw_cgi(git_cmd, "http-backend", timeout = git_timeout)

  handler <- function(req, res) {
    env <- c(
      GIT_CONFIG_GLOBAL = req$app$locals$git_config,
      GIT_HTTP_EXPORT_ALL = "true",
      GIT_PROJECT_ROOT = req$app$locals$git_root,
      GIT_PROTOCOL = req$get_header("Git-Protocol") %||% "",
      HTTP_GIT_PROTOCOL = req$get_header("Git-Protocol") %||% ""
    )
    cgi(req, res, env = env)
  }

  re_all <- new_regexp("^(?<path>.*)$")

  app$get(re_all, handler)
  app$post(re_all, handler)

  app
}
