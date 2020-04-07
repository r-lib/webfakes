
#' A presser request object
#'
#' presser creates a `presser_request` object for every incoming HTTP
#' request. This object is passed to every matched route and middleware,
#' until the response is sent. It has reference semantics, so handlers
#' can modify it.
#'
#' Fields and methods:
#'
#' * `app`: The `presser_app` object itself.
#' * `headers`: Named list of HTTP request headers.
#' * `hostname`: Client host.
#' * `method`: HTTP method.
#' * `path`: Server path.
#' * `protocol`: `"http"` or `"https"`.
#' * `query_string`: The raw query string, without the starting `?`.
#' * `query`: Parsed query parameters in a named list.
#' * `remote_addr`: String, the domain name or IP address of the client.
#'   Since presser always runs on the localhost, this is `127.0.0.1`.
#' * `url`: The full URL of the request.
#' * `get_header(field)`: Function to query a request header. Returns
#'   `NULL` if the header is not present.
#'
#' Body parsing middleware adds additional fields to the request object.
#' See [mw_raw()], [mw_text()], [mw_json()], [mw_multipart()] and
#' [mw_urlencoded()].
#'
#' @seealso [presser_response] for the presser response object.
#' @name presser_request
NULL

new_request <- function(app, rreq) {
  parsed_headers <- parse_headers(rreq$headers)
  self <- new_object(
    "presser_request",

    app = app,
    headers = parsed_headers,
    hostname = parsed_headers$host,
    method = tolower(rreq$method),
    path = rreq$local_uri,
    protocol = "http",
    query_string = rreq$query_string,
    query = parse_query(rreq$query_string),
    remote_addr = rreq$remote_addr,
    url = rreq$request_link,

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
  keys <- vapply(argstr, function(x) utils::URLdecode(x[[1]]), character(1))
  vals <- lapply(argstr, function(x) utils::URLdecode(x[[2]]))
  structure(vals, names = keys)
}
