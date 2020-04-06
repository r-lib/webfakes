
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
    query = parse_query(rreq$query_string),

    get_header = function(field) self$headers[[field]],

    .body = rreq$body
  )

  rm(parsed_headers)
  self
}

parse_headers <- function(headers) {
  names(headers) <- tolower(headers)
  headers
}

parse_query <- function(query) {
  if (length(query) == 0) {
    structure(list(), names = character())
  } else {
    as.list(query)
  }
}
