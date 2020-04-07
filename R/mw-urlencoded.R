
#' @export

mw_urlencoded <- function(type = "application/x-www-form-urlencoded") {
  function(req, res) {
    ct <- req$get_header("content-type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$form <- parse_query(rawToChar(req$.body))
    "next"
  }
}
