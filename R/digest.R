
crc32 <- function(x) {
  if (is.character(x)) x <- charToRaw(x)
  stopifnot(is.raw(x))
  .Call(c_presser_crc32, x)
}
