#' Parse a multipart HTTP request body
#'
#' Adds the parsed form fields in the `form` element of the request and
#' the parsed files to the `files` element.
#'
#' @param type Content type to match before parsing. If it does not
#'   match, then the request object is not modified.
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' app <- new_app()
#' app$use(mw_multipart())
#' app

mw_multipart <- function(type = "multipart/form-data") {
  type
  function(req, res) {
    ct <- req$get_header("Content-Type") %||% ""
    if (
      !any(vapply(
        paste0("^", type),
        function(x) grepl(x, ct),
        logical(1)
      ))
    )
      return("next")

    parts <- str_trim(strsplit(ct, ";", fixed = TRUE)[[1]])
    bnd <- grep("boundary=", parts, value = TRUE)[1]
    if (is.na(bnd)) return("next")
    bnd <- sub("^boundary=", "", bnd)

    tryCatch(
      {
        mp <- parse_multipart(req$.body, bnd)
        req$form <- list()
        req$files <- list()
        for (p in mp) {
          if (is.null(p$filename)) {
            req$form[[p$name]] <- rawToChar(p$value)
          } else {
            req$files[[p$name]] <- list(filename = p$filename, value = p$value)
          }
        }
      },
      error = function(err) NULL
    )

    "next"
  }
}

parse_multipart <- function(body, boundary) {
  boundary <- paste0("--", boundary)
  boundary_length <- nchar(boundary)

  # Find the locations of the boundary string
  indexes <- grepRaw(boundary, body, fixed = TRUE, all = TRUE)

  if (!length(indexes)) stop("Boundary was not found in the body.")

  if (length(indexes) == 1) {
    if (length(body) < (boundary_length + 5)) {
      # Empty HTML5 FormData object
      return(list())
    } else {
      # Something went wrong
      stop(
        "The 'boundary' was only found once in the ",
        "multipart/form-data message. It should appear at ",
        "least twice. The request-body might be truncated."
      )
    }
  }

  parts <- list()
  for (i in seq_along(utils::head(indexes, -1))) {
    from <- indexes[i] + boundary_length
    to <- indexes[i + 1] - 1
    parts[[i]] <- body[from:to]
  }

  out <- lapply(parts, multipart_sub)
  names(out) <- vapply(
    out,
    function(x) as.character(x$name),
    character(1)
  )

  out
}

multipart_sub <- function(bodydata) {
  splitchar <- grepRaw("\\r\\n\\r\\n|\\n\\n|\\r\\r", bodydata)
  if (!length(splitchar)) {
    stop("Invalid multipart subpart:\n\n", rawToChar(bodydata))
  }

  headers <- bodydata[1:(splitchar - 1)]
  headers <- str_trim(rawToChar(headers))
  headers <- gsub("\r\n", "\n", headers)
  headers <- gsub("\r", "\n", headers)
  headerlist <- unlist(lapply(strsplit(headers, "\n")[[1]], str_trim))

  dispindex <- grep("^Content-Disposition:", headerlist)
  if (!length(dispindex)) {
    stop("Content-Disposition header not found:", headers)
  }
  dispheader <- headerlist[dispindex]

  #get parameter name
  m <- regexpr("; name=\\\"(.*?)\\\"", dispheader)
  if (m < 0) stop('failed to find the name="..." header')

  namefield <- unquote(sub(
    "; name=",
    "",
    regmatches(dispheader, m),
    fixed = TRUE
  ))

  #test for file upload
  m <- regexpr("; filename=\\\"(.*?)\\\"", dispheader)
  if (m < 0) {
    filenamefield = NULL
  } else {
    filenamefield <- unquote(sub(
      "; filename=",
      "",
      regmatches(dispheader, m),
      fixed = TRUE
    ))
  }

  #filedata
  splitval <- grepRaw("\\r\\n\\r\\n|\\n\\n|\\r\\r", bodydata, value = TRUE)
  start <- splitchar + length(splitval)
  if (identical(utils::tail(bodydata, 2), charToRaw("\r\n"))) {
    end <- length(bodydata) - 2
  } else {
    end <- length(bodydata) - 1
  }

  #the actual fields
  list(
    name = namefield,
    value = bodydata[start:end],
    filename = filenamefield
  )
}
