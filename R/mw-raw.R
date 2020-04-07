
#' @export

mw_raw <- function(type = "application/octet-stream") {
  function(req, res) {
    ct <- req$get_header("content-type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$raw <- req$.body
    "next"
  }
}
