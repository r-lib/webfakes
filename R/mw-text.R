
#' @export

mw_text <- function(default_charset = "utf-8",
                     type = "text/plain") {
  default_charset; type
  function(req, res) {
    ct <- req$get_header("content-type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$text <- rawToChar(req$.body %||% raw())
    Encoding(req$text) <- default_charset
    "next"
  }
}
