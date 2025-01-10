
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
  cat(..., sep = sep, append = TRUE)
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

set_name <- function(x, nm) {
  names(x) <- nm
  x
}

random_id <- generate_token <- function(n = 30) {
  paste0(sample(c(0:9, letters[1:6]), n, replace = TRUE), collapse = "")
}

parse_url <- function(url) {
  re_url <- paste0(
    "^(?<protocol>[a-zA-Z0-9]+)://",
    "(?:(?<username>[^@/:]+)(?::(?<password>[^@/]+))?@)?",
    "(?<host>[^/]+)",
    "(?<path>.*)$"            # don't worry about query params here...
  )

  re_match(url, re_url)$groups
}

modify_path <- function(url, path) {
  purl <- parse_url(url)
  has_usr <- nzchar(purl$username)
  has_pwd <- nzchar(purl$password)
  paste0(
    purl$protocol,
    "://",
    purl$username,
    if (has_pwd) ":",
    purl$password,
    if (has_usr) "@",
    purl$host,
    if (substr(path, 1, 1) != "/") "/",
    path
  )
}

has_seed <- function() {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

get_seed <- function() {
  if (has_seed()) {
    get(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  }
}

set_seed <- function(seed) {
  if (is.null(seed)) {
    rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", seed, globalenv())
  }
}

local_options <- function(.new = list(), ...,
                          .local_envir = parent.frame()) {
  .new <- utils::modifyList(as.list(.new), list(...))
  old <- do.call(options, .new)
  defer(do.call(options, old), .local_envir)
}

map_chr <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

#' Format a time stamp for HTTP
#'
#' @param t Date-time value to format, defaults to the current date and
#'   time. It must be a POSIXct object.
#' @return Character vector, formatted date-time.
#' @export

http_time_stamp <- function(t = Sys.time()) {
  t <- as.POSIXlt(t, tz = "UTC")
  strftime(t, "%a, %d %b %Y %H:%M:%S GMT")
}

capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
