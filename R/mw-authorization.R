parse_authorization_header <- function(x) {
  if (length(x) == 0) return(NULL)

  scheme <- tolower(sub("[ ].*$", "", x))
  rest <- trimws(sub("^[^ ]+[ ]", "", x))

  if (scheme == "basic") {
    username <- password <- NULL
    tryCatch(
      {
        ptxt <- strsplit(base64_decode(rest), ":", fixed = TRUE)[[1]]
        if (length(ptxt) == 2) {
          username <- ptxt[1]
          password <- ptxt[2]
        }
      },
      error = function(err) NULL
    )

    if (!is.null(username) && !is.null(password)) {
      return(list(
        scheme = scheme,
        username = username,
        password = password
      ))
    } else {
      return(NULL)
    }
  }

  # if it has a (non-trailing) =, then it is a dictionary, otherwise token
  if (grepl("=", sub("=+$", "", rest))) {
    return(c(list(scheme = scheme), parse_dict_header(rest)))
  }

  list(scheme = scheme, token = rest)
}

parse_dict_header <- function(x) {
  result <- list()
  for (item in parse_list_header(x)) {
    if (!grepl("=", item)) {
      result[item] <- list(NULL)
      next
    }
    key <- sub("=.*$", "", item)
    value <- sub("^[^=]+=", "", item)

    # https://www.rfc-editor.org/rfc/rfc2231#section-4
    # we always assume UTF-8
    if (grepl("[*]$", key)) {
      key <- sub("[*]$", "", key)
      value <- sub("^.*'.*'", "", value)
      value <- utils::URLdecode(value)
      Encoding(value) <- "UTF-8"
    }

    if (grepl('^".*"$', value)) {
      value <- substr(value, 2, nchar(value) - 1L)
    }

    result[[key]] <- value
  }

  result
}

parse_list_header <- function(x) {
  s <- strsplit(x, "")[[1]]
  res <- character()
  part <- character()
  escape <- quote <- FALSE

  for (cur in s) {
    if (escape) {
      part[length(part) + 1L] <- cur
      escape <- FALSE
      next
    }

    if (quote) {
      if (cur == "\\") {
        escape <- TRUE
        next
      } else if (cur == '"') {
        quote <- FALSE
      }
      part[length(part) + 1L] <- cur
      next
    }

    if (cur == ",") {
      res[length(res) + 1L] <- paste(part, collapse = "")
      part <- character()
      next
    }

    if (cur == '"') {
      quote <- TRUE
    }

    part[length(part) + 1L] <- cur
  }

  if (length(part)) {
    res[length(res) + 1L] <- paste(part, collapse = "")
  }

  trimws(res)
}
