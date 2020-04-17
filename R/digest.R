
crc32 <- function(x) {
  if (is.character(x)) x <- charToRaw(x)
  stopifnot(is.raw(x))
  call_with_cleanup(c_presser_crc32, x)
}
