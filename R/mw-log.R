
#' Log requests to the standard output or other connection
#'
#' A one line log entry for every request. The output looks like this:
#' ```
#' GET http://127.0.0.1:3000/image 200 3 ms - 4742
#' ```
#' and contains
#' * the HTTP method,
#' * the full request URL,
#' * the HTTP status code of the response,
#' * how long it took to process the response, in ms,
#' * and the size of the response body, in bytes.
#'
#' @param format Log format. Not implemented currently.
#' @param stream R connection to log to. Defaults to `stdout()`, the
#'   standard output.
#'
#' @family middleware
#' @export

mw_log <- function(format = "dev", stream = stdout()) {

  format; stream

  function(req, res) {

    start <- Sys.time()

    fmt <- function(req, res) {
      len <- if (is.null(res$.body)) {
        0L
      } else if (is.raw(res$.body)) {
        length(res$.body)
      } else if (is_string(res$.body)) {
        nchar(res$.body, type = "bytes")
      } else {
        "??"
      }
      t <- as.integer(round((Sys.time() - start) * 1000))
      msg <- sprintf(
        "%s %s %s %s ms - %s\n",
        toupper(req$method), req$url, res$.status, t, len
      )
      cat0(msg, file = stream)
      flush(stream)
    }
    res$on_response(fmt)

    "next"
  }
}
