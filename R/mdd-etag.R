
#' @export

mdd_etag <- function(algorithm = "crc32") {
  algorithm
  function(req, res) {

    do <- function(req, res) {
      if (length(res$.body) == 0) {
        etag <- "\"0-2jmj7l5rSw0yVb/vlWAYkK/YBwk\""
      } else {
        etag <- paste0(
          "\"",
          digest::digest(res$.body, algo = algorithm,
                         serialize = FALSE),
          "\""
        )
      }
      res$set_header("etag", etag)
    }
    res$on_response(do)

    "next"
  }
}
