
#' @export

httpbin_app <- function(log = TRUE) {

  app <- new_app()

  if (log) app$use(mdd_log())
  app$use(mdd_json())
  app$use(mdd_text(type = c("text/plain", "application/json")))
  app$use(mdd_etag())

  # HTTP methods =========================================================

  app$get("/get", function(req, res) {
    ret <- list(
      args = as.list(req$query),
      headers = req$headers,
      path = req$path
    )
    res$send_json(object = ret, auto_unbox = TRUE, pretty = TRUE)
  })

  app$post("/post", function(req, res) {
    ## TODO: parse other body types
    ret <- list(
      args = as.list(req$query),
      headers = req$headers,
      path = req$path
    )
    if (!is.null(req$text)) ret$data <- req$text
    if (!is.null(req$json)) ret$json <- req$json

    res$send_json(object = ret, auto_unbox = TRUE, pretty = TRUE)
  })

  # Auth =================================================================

  app$get("/basic-auth/:user/:passwd", function(req, res) {
    exp <- paste(
      "Basic",
      base64_encode(paste0(req$params$user, ":", req$params$passwd))
    )
    hdr <- req$get_header("authorization") %||% ""
    if (exp == hdr) {
      res$send_json(list(
        authenticated = jsonlite::unbox(TRUE),
        user = jsonlite::unbox(req$params$user)
      ))
    } else {
      res$
        set_header("WWW-Authenticate", "Basic realm=\"Fake Realm\"")$
        send_status(401)
    }
  })

  # Status codes =========================================================

  app$all(
    new_regexp("^/status/(?<status>[0-9][0-9][0-9])$"),
    function(req, res) {
      res$send_status(req$params$status)
    }
  )

  # Request inspection ===================================================

  # Response inspection ==================================================

  # Response formats =====================================================

  # Dynamic data =========================================================

  # Cookies ==============================================================

  # Redirects ============================================================

  app$all("/redirect-to", function(req, res) {
    res$redirect(req$query$url, req$query$status_code %||% 302)
  })

  # Anything =============================================================

  app
}
