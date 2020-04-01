
#' @export

mdd_text <- function(default_charset = "utf-8",
                     inflate = TRUE,
                     limit = 100 * 1000,
                     type = "text/plain") {
  # TODO: implement inflate, limit
  default_charset; inflate; limit; type
  function(req, res) {
    ct <- req$get_header("content-type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$text <- rawToChar(req$.body %||% raw())
    Encoding(req$text) <- default_charset
    "next"
  }
}
