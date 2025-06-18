#' Middleware to parse a plain text body
#'
#' Adds the parsed object as the `text` element of the request object.
#'
#' @param default_charset Encoding to set on the text.
#' @param type Content type to match before parsing. If it does not
#'   match, then the request object is not modified.
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_text())
#' app

mw_text <- function(default_charset = "utf-8", type = "text/plain") {
  default_charset
  type
  function(req, res) {
    ct <- req$get_header("Content-Type") %||% ""
    if (!ct %in% tolower(type)) {
      return("next")
    }
    req$text <- rawToChar(req$.body %||% raw())
    Encoding(req$text) <- default_charset
    "next"
  }
}
