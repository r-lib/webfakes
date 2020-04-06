
new_request <- function(app, rreq) {
  parsed_headers <- parse_headers(rreq$headers)
  self <- new_object(
    "press_request",

    app = app,
    base_url = app$path(),
    headers = parsed_headers,
    hostname = parsed_headers$host,
    method = tolower(rreq$method),
    path = rreq$local_uri,
    protocol = "http",
    query_string = rreq$query_string,
    query = parse_query(rreq$query_string),

    get_header = function(field) self$headers[[field]],

    .body = rreq$body
  )

  rm(parsed_headers)
  self
}

parse_headers <- function(headers) {
  names(headers) <- tolower(names(headers))
  headers
}

parse_query <- function(query) {
  query <- sub("^[?]", "", query)
  query <- chartr("+", " ", query)
  argstr <- strsplit(query, "&", fixed = TRUE)[[1]]
  argstr <- strsplit(argstr, "=", fixed = TRUE)
  keys <- vapply(argstr, function(x) URLdecode(x[[1]]), character(1))
  vals <- lapply(argstr, function(x) URLdecode(x[[2]]))
  structure(vals, names = keys)
}
