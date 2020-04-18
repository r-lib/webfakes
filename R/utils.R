
`%||%` <- function(l, r) if (is.null(l)) r else l

`%|NA|%` <- function(l, r) if (is_na_scalar(l)) r else l

new_object <- function(class_name, ...) {
  structure(as.environment(list(...)), class = class_name)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_integerish <- function(x) {
  is.integer(x) || (is.numeric(x) && all(round(x) == x))
}

is_count <- function(x) {
  is_integerish(x) && length(x) == 1 && !is.na(x)
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

read_char <- function(path, encoding = "UTF-8") {
  txt <- rawToChar(readBin(path, "raw", file.info(path)$size))
  Encoding(txt) <- encoding
  txt
}

read_bin <- function(path) {
  readBin(path, "raw", file.info(path)$size)
}

try_silently <- function(expr) {
  try(expr, silent = TRUE)
}

sseq <- function(from, to) {
  if (from > to) integer() else seq(from, to)
}

is.named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}

set_envvar <- function(envs) {
  if (length(envs) == 0) return()

  stopifnot(is.named(envs))

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  both_set <- set & !is.na(old)

  if (any(set))  do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}

mkdirp <- function(path, ...) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE, ...)
}

strrep <- function(x, no) {
  paste(rep(x, no), collapse = "")
}
