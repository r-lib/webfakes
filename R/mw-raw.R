
#' Middleware to read the raw body of a request
#'
#' Adds the raw body, as a raw object to the `raw` field of the request.
#'
#' @param type Content type to match. If it does not match, then the
#' request object is not modified.
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_raw())
#' app

mw_raw <- function(type = "application/octet-stream") {
  function(req, res) {
    ct <- req$get_header("Content-Type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$raw <- req$.body
    "next"
  }
}
