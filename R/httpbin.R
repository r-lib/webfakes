
#' @export

httpbin_app <- function(log = TRUE) {

  encode_files <- function(files) {
    for (i in seq_along(files)) {
      files[[i]]$value <- paste0(
        "data:application/octet-stream;base64,",
        base64_encode(files[[i]]$value)
      )
    }
    files
  }

  app <- new_app()

  if (log) app$use(mw_log())
  app$use(mw_json())
  app$use(mw_text(type = c("text/plain", "application/json")))
  app$use(mw_multipart())
  app$use(mw_etag())

  # HTTP methods =========================================================

  app$get("/get", function(req, res) {
    ret <- list(
      args = as.list(req$query),
      headers = req$headers,
      origin = req$remote_addr,
      path = req$path,
      url = req$url
    )
    res$send_json(object = ret, auto_unbox = TRUE, pretty = TRUE)
  })

  http_methods <- function(req, res) {
    ret <- list(
      args = as.list(req$query),
      data = req$text,
      files = encode_files(req$files),
      form = req$form,
      headers = req$headers,
      json = req$json,
      path = req$path,
      origin = req$remote_addr,
      url = req$url
    )
    res$send_json(object = ret, auto_unbox = TRUE, pretty = TRUE)
  }

  app$delete("/delete", http_methods)
  app$patch("/patch", http_methods)
  app$post("/post", http_methods)
  app$put("/put", http_methods)

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
