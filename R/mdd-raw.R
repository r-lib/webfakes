
#' @export

mdd_raw <- function(inflate = TRUE,
                    limit = 100 * 1000,
                    type = "application/octet-stream") {
  # TODO: implement inflate and limit
  function(req, res) {
    ct <- req$get_header("content-type") %||% ""
    if (! ct %in% tolower(type)) return("next")
    req$raw <- req$.body
    "next"
  }  
}
