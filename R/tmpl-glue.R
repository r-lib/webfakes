#' glue based template engine
#'
#' Use this template engine to create pages with glue templates.
#' See [glue::glue()] for the syntax.
#'
#' @param sep Separator used to separate elements.
#' @param open The opening delimiter. Doubling the full delimiter escapes
#'    it.
#' @param close The closing delimiter. Doubling the full delimiter escapes
#'    it.
#' @param na Value to replace NA values with. If `NULL` missing values are
#'    propagated, that is an `NA` result will cause `NA` output.
#'    Otherwise the value is replaced by the value of `na`.
#' @param transformer A function taking three parameters `code`, `envir`
#'    and `data` used to transform the output of each block before during or
#'    after evaluation.
#' @param trim Whether to trim the input template with [glue::trim()] or not.
#' @return Template function.
#'
#' @export
#' @examples
#' # See th 'hello' app at
#' hello_root <- system.file(package = "webfakes", "examples", "hello")
#' hello_root
#'
#' app <- new_app()
#' app$engine("txt", tmpl_glue())
#' app$use(mw_log())
#'
#'
#' app$get("/view", function(req, res) {
#'   txt <- res$render("test")
#'   res$
#'     set_type("text/plain")$
#'     send(txt)
#' })
#'
#' # Switch to the app's root: setwd(hello_root)
#' # Now start the app with: app$listen(3000L)
#' # Or start it in another process: new_process(app)

tmpl_glue <- function(
  sep = "",
  open = "{",
  close = "}",
  na = "NA",
  transformer = NULL,
  trim = TRUE
) {
  sep
  open
  close
  na
  transformer
  trim
  function(path, locals) {
    txt <- readChar(path, nchars = file.size(path))
    glue::glue_data(
      locals,
      txt,
      .sep = sep,
      .open = open,
      .close = close,
      .na = na,
      .transformer = transformer %||% glue::identity_transformer,
      .trim = trim
    )
  }
}
