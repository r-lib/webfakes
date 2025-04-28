uuid_random <- function() {
  # These uuids are pseudo-random, they are not secure!
  # xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx (8-4-4-4-12)
  # The 4 bits of digit M are the UUID version,
  # and the 1 to 3 most significant bits of digit N code the UUID variant.
  digits <- floor(stats::runif(32, 0, 16))
  digits[13] <- 0x4
  digits[17] <- bitwOr(bitwAnd(digits[17], 0x3), 0x8)

  x <- format(as.hexmode(digits))
  paste(
    collapse = "",
    c(x[1:8], "-", x[9:12], "-", x[13:16], "-", x[17:20], "-", x[21:32])
  )
}
