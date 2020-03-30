
#' @export

new_regexp <- function(x) structure(x, class = "pressr_regexp")

path_match <- function(method, path, handler) {
  if (handler$method == "use") return(TRUE)
  if (! handler$method %in% c("all", method)) return(FALSE)
  pattern_match(path, handler$path)
}

pattern_match <- function(path, patterns) {

  # Make sure patterns is a list
  if (inherits(patterns, "pressr_regexp")) {
    patterns <- list(patterns)
  } else if (is.character(patterns)) {
    patterns <- as.list(patterns)
  }

  for (p in patterns) {
    if (inherits(p, "pressr_regexp")) {
      m <- re_match(path, p)
      if (!anyNA(m)) return(list(params = m))
    } else {
      if (path == p) return(TRUE)
    }
  }

  FALSE
}
