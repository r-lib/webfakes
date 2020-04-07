
#' @export

mw_json <- function(type = "application/json",
                     simplifyVector = FALSE,
                     ...) {
  type; simplifyVector; list(...)
  function(req, res) {
    ct <- req$get_header("content-type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$json <- jsonlite::fromJSON(
      rawToChar(req$.body),
      simplifyVector = simplifyVector,
      ...
    )
    "next"
  }
}
