#' Middleware to parse a JSON body
#'
#' Adds the parsed object as the `json` element of the request object.
#'
#' @param type Content type to match before parsing. If it does not
#'   match, then the request object is not modified.
#' @param simplifyVector Whether to simplify lists to vectors, passed to
#'   [jsonlite::fromJSON()].
#' @param ... Arguments to pass to [jsonlite::fromJSON()], that performs
#'   the JSON parsing.
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_json())
#' app

mw_json <- function(type = "application/json", simplifyVector = FALSE, ...) {
  type
  simplifyVector
  list(...)
  function(req, res) {
    ct <- req$get_header("Content-Type") %||% ""
    if (!ct %in% tolower(type)) return("next")
    if (!is.null(req$.body)) {
      req$json <- jsonlite::fromJSON(
        rawToChar(req$.body),
        simplifyVector = simplifyVector,
        ...
      )
    }
    "next"
  }
}
