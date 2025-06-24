#' Middleware to parse an url-encoded request body
#'
#' This is typically data from a form. The parsed data is added
#' as the `form` element of the request object.
#'
#' @param type Content type to match before parsing. If it does not
#'   match, then the request object is not modified.
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_urlencoded())
#' app

mw_urlencoded <- function(type = "application/x-www-form-urlencoded") {
  function(req, res) {
    ct <- req$get_header("Content-Type") %||% ""
    if (!ct %in% tolower(type)) {
      return("next")
    }
    if (!is.null(req$.body)) {
      req$form <- parse_query(rawToChar(req$.body))
    }
    "next"
  }
}
