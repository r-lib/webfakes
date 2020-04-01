
XX <- 255L
EQ <- 254L
INVALID <- XX

index_64 <- as.integer(c(
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,62, XX,XX,XX,63,
    52,53,54,55, 56,57,58,59, 60,61,XX,XX, XX,EQ,XX,XX,
    XX, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,XX, XX,XX,XX,XX,
    XX,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,XX, XX,XX,XX,XX,

    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX
))

base64_decode <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }

  len <- length(x)
  idx <- 1
  c <- integer(4)
  out <- raw()
  while(idx <= len) {
    i <- 1
    while(i <= 4) {
      uc <- index_64[[as.integer(x[[idx]]) + 1L]]
      idx <- idx + 1
      if (uc != INVALID) {
        c[[i]] <- uc
        i <- i + 1
      }
      if (idx > len) {
        if (i <= 4) {
          if (i <= 2) return(rawToChar(out))
          if (i == 3) {
            c[[3]] <- EQ
            c[[4]] <- EQ
          }
          break
        }
      }
  }

    if (c[[1]] == EQ || c[[2]] == EQ) {
      break
    }

    #print(sprintf("c1=%d,c2=%d,c3=%d,c4=%d\n", c[1],c[2],c[3],c[4]))

    out[[length(out) + 1]] <- as.raw(bitwOr(bitwShiftL(c[[1]], 2L), bitwShiftR(bitwAnd(c[[2]], 0x30), 4L)))

    if (c[[3]] == EQ) {
      break
    }

    out[[length(out) + 1]] <- as.raw(bitwOr(bitwShiftL(bitwAnd(c[[2]], 0x0F), 4L), bitwShiftR(bitwAnd(c[[3]], 0x3C), 2L)))

    if (c[[4]] == EQ) {
      break
    }

    out[[length(out) + 1]] <- as.raw(bitwOr(bitwShiftL(bitwAnd(c[[3]], 0x03), 6L), c[[4]]))
  }
  rawToChar(out)
}

basis64 <- charToRaw(paste(c(LETTERS, letters, 0:9, "+", "/"),
                           collapse = ""))

base64_encode <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }

  len <- length(x)
  rlen <- floor((len + 2L) / 3L) * 4L
  out <- raw(rlen)
  ip <- op <- 1L
  c <- integer(4)

  while (len > 0L) {
    c[[1]] <- as.integer(x[[ip]])
    ip <- ip + 1L
    if (len > 1L) {
      c[[2]] <- as.integer(x[ip])
      ip <- ip + 1L
    } else {
      c[[2]] <- 0L
    }
    out[op] <- basis64[1 + bitwShiftR(c[[1]], 2L)]
    op <- op + 1L
    out[op] <- basis64[1 + bitwOr(bitwShiftL(bitwAnd(c[[1]], 3L), 4L),
                                  bitwShiftR(bitwAnd(c[[2]], 240L), 4L))]
    op <- op + 1L

    if (len > 2) {
      c[[3]] <- as.integer(x[ip])
      ip <- ip + 1L
      out[op] <- basis64[1 + bitwOr(bitwShiftL(bitwAnd(c[[2]], 15L), 2L),
                                    bitwShiftR(bitwAnd(c[[3]], 192L), 6L))]
      op <- op + 1L
      out[op] <- basis64[1 + bitwAnd(c[[3]], 63)]
      op <- op + 1L

    } else if (len == 2) {
      out[op] <- basis64[1 + bitwShiftL(bitwAnd(c[[2]], 15L), 2L)]
      op <- op + 1L
      out[op] <- charToRaw("=")
      op <- op + 1L

    } else { ## len == 1
      out[op] <- charToRaw("=")
      op <- op + 1L
      out[op] <- charToRaw("=")
      op <- op + 1L

    }
    len <- len - 3L
  }

  rawToChar(out)
}
