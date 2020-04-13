
#' Create a new regular expression to use in presser routes
#'
#' Note that presser uses PERL regular expressions.
#'
#' @details
#' As R does not have data type or class for regular expressions,
#' you can use `new_regexp()` to mark a string as a regular expression,
#' when adding routes.
#'
#' @param x String scalar containing a regular expression.
#' @return String with class `presser_regexp`.
#'
#' @aliases presser_regexp
#' @seealso The 'Path specification' and 'Path parameters' chapters
#' of the manual of [new_app()].
#' @export
#' @examples
#' new_regexp("^/api/match/(?<pattern>.*)$")

new_regexp <- function(x) structure(x, class = "presser_regexp")

path_match <- function(method, path, handler) {
  if (handler$method == "use") return(TRUE)
  if ((! handler$method %in% c("all", method)) &&
      !(handler$method == "get" && method == "head")) return(FALSE)
  pattern_match(path, handler$path)
}

pattern_match <- function(path, patterns) {

  # Make sure patterns is a list
  if (inherits(patterns, "presser_regexp")) {
    patterns <- list(patterns)
  } else if (is.character(patterns)) {
    patterns <- as.list(patterns)
  }

  for (p in patterns) {
    if (!inherits(p, "presser_regexp") && grepl(":", p)) {
      p <- path_to_regexp(p)
    }
    if (inherits(p, "presser_regexp")) {
      m <- re_match(path, p)
      if (m$match) return(list(params = as.list(m$groups)))
    } else {
      if (path == p) return(TRUE)
    }
  }

  FALSE
}

path_to_regexp <- function(path) {
  tokens <- strsplit(path, "/")[[1]]
  keys <- grep("^:", tokens)
  reg <- tokens
  reg[keys] <- paste0("(?<", substring(tokens[keys], 2), ">[A-Za-z0-9_]+)")
  new_regexp(paste0("^", paste(reg, collapse = "/"), "$"))
}
