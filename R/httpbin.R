
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

  # TODO: other methods, once we have a proper web server

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

  # TODO: /bearer * /digest-auth * /hidden-basic-auth

  # Status codes =========================================================

  app$all(
    new_regexp("^/status/(?<status>[0-9][0-9][0-9])$"),
    function(req, res) {
      res$send_status(req$params$status)
    }
  )

  # Request inspection ===================================================

  # TODO: /headers * /ip * /user-agent

  # Response inspection ==================================================

  # TODO: /cache * /cache/{value} * /etag * /response-headers (2x)

  # Response formats =====================================================

  # TODO: /brotli * /deflate * /deny * /encoding/utf8 * /gzip * /html *
  # /json * /robots.txt * /xml

  # Dynamic data =========================================================

  # TODO: /base64/{value} * /bytes/{n} * /delay/{delay} * /drip *
  # /links/{n}{offset} * /range/{numbytes} * /stream-bytes/{n} *
  # /stream/{n} * /uuid

  # Cookies ==============================================================

  # TODO: /cookies * /cookies/delete * /cookies/set *
  # /cookies/set/{name}/{value}

  # Images ===============================================================

  # TODO: /image * /image/jpeg * /image/png * /image/svg * /image/webp

  # Redirects ============================================================

  app$all("/redirect-to", function(req, res) {
    res$redirect(req$query$url, req$query$status_code %||% 302)
  })

  # TODO: /absolute-redirect/{n} * /redirect/{n} *
  # relative-redirect/{n}

  # Anything =============================================================

  # TODO: /anything * /anything/{anything}

  app
}
