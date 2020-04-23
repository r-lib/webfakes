
#' Middleware that add an `Etag` header to the response
#'
#' @param algorithm Checksum algorithm to use. Only `"crc32"` is
#' implemented currently.
#'
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_etag())
#' app

mw_etag <- function(algorithm = "crc32") {
  if (algorithm != "crc32") {
    stop("Only the 'crc32' algorithm is implemented in `mw_etag()`")
  }
  function(req, res) {
    do <- function(req, res) {
      etag <- paste0("\"", crc32(res$.body), "\"")
      res$set_header("Etag", etag)
    }
    res$on_response(do)

    "next"
  }
}
