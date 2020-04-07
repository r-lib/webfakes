
#' @export

mw_log <- function(format = "dev", stream = stdout()) {

  format; stream

  function(req, res) {

    start <- Sys.time()

    fmt <- function(req, res) {
      len <- if (is.raw(res$.body)) {
        length(res$.body)
      } else if (identical(names(res$.body), "file")) {
        file.info(res$.body)$size
      } else {
        nchar(res$.body, type = "bytes")
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
