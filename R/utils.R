
`%||%` <- function(l, r) if (is.null(l)) r else l

new_object <- function(class_name, ...) {
  structure(as.environment(list(...)), class = class_name)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_count <- function(x) {
  is.integer(x) && length(x) == 1 && !is.na(x)
}

is_port <- function(x) {
  is_count(x)
}

is_na_scalar <- function(x) {
  is.atomic(x) && length(x) == 1 && is.na(x)
}

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

unquote <- function(str) {
  len <- nchar(str)
  if (substr(str, 1, 1) == '"' && substr(str, len, len) == '"') {
    substr(str, 2, len - 1)

  } else {
    str
  }
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

str_is_suffix <- function(x, sfx) {
  lx <- nchar(x)
  lsfx <- nchar(sfx)
  substring(x, lx - lsfx + 1, lx) == sfx
}
