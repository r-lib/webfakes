
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

tmpl_glue <- function(sep = "", open = "{", close = "}", na = "NA",
                      transformer = NULL, trim = TRUE) {
  sep; open; close; na; transformer; trim
  function(path, locals) {
    txt <- readChar(path, nchars = file.size(path), useBytes = TRUE)
    glue::glue_data(
      locals, txt, .sep = sep, .open = open, .close = close, .na = na,
      .transformer = transformer %||% glue::identity_transformer,
      .trim = trim
    )
  }
}
