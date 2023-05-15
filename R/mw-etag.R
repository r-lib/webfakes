
#' Middleware that add an `ETag` header to the response
#'
#' If the response already has an `ETag` header, then it is kept.
#'
#' This middleware handles the `If-None-Match` headers, and it sets the
#' status code of the response to 304 if `If-None-Match` matches the
#' `ETag`. It also removes the response body in this case.
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
      etag <- res$get_header("ETag")
      if (is.null(etag)) {
        etag <- paste0("\"", crc32(res$.body), "\"")
        res$set_header("ETag", etag)
      }
      req_etag <- req$get_header("If-None-Match")
      if (!is.null(req_etag) && req_etag == etag) {
        res$.body <- NULL
        res$set_status(304)
      }
    }
    res$on_response(do)

    "next"
  }
}
