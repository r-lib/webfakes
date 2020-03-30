
new_request <- function(app, path, query, body, headers) {
  parsed_headers <- parse_headers(headers)
  self <- new_object(
    "press_request",

    app = app,
    base_url = app$path(),
    headers = parsed_headers,
    hostname = parsed_headers$host,
    method = if (is.null(body)) "get" else "post",
    path = path,
    protocol = "http",
    query = parse_query(query),

    get_header = function(field) self$headers[[field]],

    .body = body
  )

  rm(parsed_headers)
  self
}

parse_headers <- function(headers) {
  txt <- rawToChar(headers)
  sets <- strsplit(txt, "\\r\\n\\r\\n|\\n\\n|\\r\\r")[[1]]
  h1 <- strsplit(sets, "\\r\\n|\\n|\\r")[[1]]
  h2 <- grep(":", h1, fixed = TRUE, value = TRUE)
  out <- strsplit(h2, ":", fixed = TRUE)
  names <- tolower(vapply(out, `[[`, character(1), 1))
  values <- lapply(lapply(out, `[[`, 2), trimws)
  names(values) <- names
  values
}

parse_query <- function(query) {
  if (length(query) == 0) {
    structure(list(), names = character())
  } else {
    as.list(query)
  }
}
