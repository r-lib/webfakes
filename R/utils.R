
`%||%` <- function(l, r) if (is.null(l)) r else l

new_object <- function(class_name, ...) {
  structure(as.environment(list(...)), class = class_name)
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
