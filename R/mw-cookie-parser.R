#' Middleware to parse Cookies
#'
#' Adds the cookies as the `cookies` element of the request object.
#'
#' It ignores cookies in an invalid format. It ignores duplicate cookies:
#' if two cookies have the same name, only the first one is included.
#'
#' @return Handler function.
#'
#' @family middleware
#' @export

mw_cookie_parser <- function() {
  function(req, res) {
    ch <- req$get_header("Cookie") %||% ""
    req$cookies <- parse_cookies(ch)
    "next"
  }
}

parse_cookies <- function(x) {
  parts <- strsplit(x, ";", fixed = TRUE)[[1]]
  dict <- structure(list(), names = character())
  lapply(parts, function(ck) {
    ck <- trimws(ck)
    key <- sub("^([^=]+)=.*$", "\\1", ck)
    if (key == ck) return()
    if (!is.null(dict[[key]])) return()
    value <- sub("^[^=]+=", "", ck)
    dict[[key]] <<- value
  })
  dict
}
