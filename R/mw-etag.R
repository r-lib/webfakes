
#' Middleware that add an `Etag` header to the response
#'
#' It needs the digest package to calculate the value of the header.
#'
#' @param algorithm Checksum algorithm to use. See the `also` argument of
#' [digest::digest()] for the possible values.
#' @return Handler function.
#'
#' @family middleware
#' @export

mw_etag <- function(algorithm = "crc32") {
  algorithm
  function(req, res) {

    do <- function(req, res) {
      if ((is.raw(res$.body) && length(res$.body) == 0) ||
          (is.character(res$.body) && nchar(res$.body) == 0) ||
          is.null(res$.body)) {
        etag <- "\"0-2jmj7l5rSw0yVb/vlWAYkK/YBwk\""
      } else {
        is_file <- identical(names(res$.body), "file")
        etag <- paste0(
          "\"",
          digest::digest(res$.body, algo = algorithm,
                         serialize = FALSE, file = is_file),
          "\""
        )
      }
      res$set_header("Etag", etag)
    }
    res$on_response(do)

    "next"
  }
}
