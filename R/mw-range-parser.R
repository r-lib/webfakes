#' Middleware to parse a Range header
#'
#' Adds the requested ranges to the `ranges` element of the request
#' object. `request$ranges` is a data frame with two columns, `from` and
#' `to`. Each row corresponds one requested interval.
#'
#' When the last `n` bytes of the file are requested, the matrix row is set
#' to `c(0, -n)`. When all bytes after a `p` position are requested, the
#' matrix row is set to `c(p, Inf)`.
#'
#' If the intervals overlap, then `ranges` is not set, i.e. the `Range`
#' header is ignored.
#'
#' If its syntax is invalid or the unit is not `bytes`, then the
#' `Range` header is ignored.
#'
#' @return Handler function.
#'
#' @family middleware
#' @export

mw_range_parser <- function() {
  function(req, res) {
    rh <- req$get_header("Range")
    if (length(rh) == 0) {
      return("next")
    }
    req$ranges <- parse_range(rh)
    "next"
  }
}

parse_range <- function(rh) {
  rh <- trimws(rh)
  if (length(rh) == 0 || !startsWith(rh, "bytes=")) {
    return()
  }
  rh <- sub("^bytes=[ ]*", "", rh)
  rngs <- trimws(strsplit(rh, ",", fixed = TRUE)[[1]])
  res <- matrix(integer(1), nrow = length(rngs), ncol = 2)

  for (i in seq_along(rngs)) {
    rng <- strsplit(rngs[i], "-")[[1]]
    if (length(rng) < 1 || length(rng) > 3) {
      return()
    } else if (length(rng) == 1) {
      res[i, 1] <- parse_int(rng[1])
      res[i, 2] <- Inf
      if (is.na(res[i, 1]) || res[i, 1] < 0) return()
    } else if (rng[1] == "") {
      res[i, 1] <- 0
      res[i, 2] <- -parse_int(rng[2])
      if (is.na(res[i, 2]) || res[i, 2] > 0) return()
    } else {
      res[i, 1] <- parse_int(rng[1])
      res[i, 2] <- parse_int(rng[2])
      if (
        is.na(res[i, 1]) ||
          is.na(res[i, 2]) ||
          res[i, 1] < 0 ||
          res[i, 2] < 0 ||
          res[i, 1] > res[i, 2]
      ) {
        return()
      }
    }
  }

  res <- res[order(res[, 1]), , drop = FALSE]

  # check for overlapping intervals
  if (intervals_overlap(res)) {
    return()
  }

  res
}

parse_int <- function(x) {
  suppressWarnings(as.integer(x))
}

intervals_overlap <- function(x) {
  # assume that it is sorted on first column
  # then every interval needs to finish before the next one starts
  if (nrow(x) <= 1) {
    return(FALSE)
  }
  any(x[, 2][-nrow(x)] >= x[, 1][-1])
}
