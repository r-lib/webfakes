
#' @export

mw_json <- function(inflate = TRUE,
                     limit = 100 * 1000,
                     type = "application/json",
                     simplifyVector = FALSE,
                     ...) {
  # TODO: implement inflate, limit
  inflate; limit; type; simplifyVector; list(...)
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
