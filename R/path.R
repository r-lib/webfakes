
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
    if (!inherits(p, "pressr_regexp") && grepl(":", p)) {
      p <- path_to_regexp(p)
    }
    if (inherits(p, "pressr_regexp")) {
      m <- re_match(path, p)
      if (m$match) return(list(params = m$groups))
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
