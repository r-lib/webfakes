
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

  # Log requests by default
  if (log) app$use(mw_log())

  # Parse all kinds of bodies
  app$use(mw_json())
  app$use(mw_text(type = c("text/plain", "application/json")))
  app$use(mw_multipart())

  # Add etags by default
  app$use(mw_etag())

  # Add date by default
  app$use(function(req, res) {
    res$set_header("date", as.character(Sys.time()))
    "next"
  })

  common_response <- function(req, res) {
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

  app$delete("/delete", common_response)
  app$patch("/patch", common_response)
  app$post("/post", common_response)
  app$put("/put", common_response)

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

  app$get("/bearer", function(req, res) {
    auth <- req$get_header("authorization") %||% ""
    if (! grepl("^Bearer ", auth)) {
      res$
        set_header("www-authenticate", "bearer")$
        send_status(401L)
    } else {
      token <- sub("^Bearer ", "", auth)
      res$
        send_json(
          list(authenticated = TRUE, token = token),
          auto_unbox = TRUE, pretty = TRUE
        )
    }
  })

  # TODO: /digest-auth * /hidden-basic-auth

  # Status codes =========================================================

  app$all(
    new_regexp("^/status/(?<status>[0-9][0-9][0-9])$"),
    function(req, res) {
      res$send_status(req$params$status)
    }
  )

  # Request inspection ===================================================

  app$get("/headers", function(req, res) {
    ret <- list(headers = req$headers)
    res$send_json(ret, auto_unbox = TRUE, pretty = TRUE)
  })

  app$get("/ip", function(req, res) {
    ret <- list(origin = req$remote_addr)
    res$send_json(ret, auto_unbox = TRUE, pretty = TRUE)
  })

  app$get("/user-agent", function(req, res) {
    ret <- list("user-agent" = req$get_header("user-agent"))
    res$send_json(ret, auto_unbox = TRUE, pretty = TRUE)
  })

  # Response inspection ==================================================

  # TODO: /cache * /cache/{value} * /etag * /response-headers (2x)

  # Response formats =====================================================

  app$get("/html", function(req, res) {
    path <- system.file(
      package = "presser", "examples", "httpbin", "data", "example.html"
    )
    res$send_file(root = "/", path)
  })

  app$get("/json", function(req, res) {
    path <- system.file(
      package = "presser", "examples", "httpbin", "data", "example.json"
    )
    res$send_file(root = "/", path)
  })

  app$get("/xml", function(req, res) {
    path <- system.file(
      package = "presser", "examples", "httpbin", "data", "example.xml"
    )
    res$send_file(root = "/", path)
  })

  app$get("/robots.txt", function(req, res) {
    path <- system.file(
      package = "presser", "examples", "httpbin", "data", "robots.txt"
    )
    res$send_file(root = "/", path)
  })

  # TODO: /brotli * /deflate * /deny * /encoding/utf8 * /gzip

  # Dynamic data =========================================================

  app$get(new_regexp("/base64/(?<value>[\\+/=a-zA-Z0-9]*)"),
          function(req, res) {
    value <- req$params$value
    if (value == "") value <- "RXZlcnl0aGluZyBpcyBSc29tZQ=="
    plain <- charToRaw(base64_decode(value))
    res$
      set_type("application/octet-stream")$
      send(plain)
  })

  app$get("/bytes/:n", function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    if (is.na(n)) {
      res$send_status(404L)
    } else {
      n <- min(n, 10000)
      bytes <- as.raw(as.integer(floor(runif(n, min=0, max=256))))
      res$
        set_type("application/octet-stream")$
        send(bytes)
    }
  })

  app$all("/delay/:delay", function(req, res) {
    delay <- suppressWarnings(as.integer(req$params$delay))
    if (is.na(delay)) {
      res$send_status(404L)
    } else {
      delay <- min(delay, 10)
      Sys.sleep(delay)
      common_response(req, res)
    }
  })

  # TODO: /drip *
  # /links/{n}{offset} * /range/{numbytes} * /stream-bytes/{n} *
  # /stream/{n} * /uuid

  # Cookies ==============================================================

  # TODO: /cookies * /cookies/delete * /cookies/set *
  # /cookies/set/{name}/{value}

  # Images ===============================================================

  app$get("/image", function(req, res) {
    act <- req$get_header("accept")
    ok <- c(
      "image/webp",
      "image/svg+xml",
      "image/jpeg",
      "image/png",
      "image/*"
    )
    msg <- list(
      message = "Client did not request a supported media type.",
      accept = ok
    )
    if (is.null(act) || ! act %in% ok) {
      res$
        set_status(406)$
        set_type("application/json")$
        send_json(msg)
    } else {
      fls <- c(
        "image/webp" = "Rlogo.webp",
        "image/svg+xml" = "Rlogo.svg",
        "image/jpeg" = "Rlogo.jpeg",
        "image/png" = "Rlogo.png",
        "image/*" = "Rlogo.png"
      )
      path <- system.file(
        package = "presser", "examples", "httpbin", "images",
        fls[act]
      )
      res$send_file(root = "/", path)
    }
  })

  app$get(new_regexp("/image/(?<format>jpeg|png|svg|webp)"),
          function(req, res) {
    path <- system.file(
      package = "presser", "examples", "httpbin", "images",
      paste0("Rlogo.", req$params$format)
    )
    res$send_file(root = "/", path)
  })

  # Redirects ============================================================

  app$get("/absolute-redirect/:n", function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    if (is.na(n)) {
      res$send_status(404L)
    } else {
      if (n == 1) {
        url <- sub("/absolute-redirect/[0-9]+$", "/get", req$url)
      } else {
        n <- min(n, 5)
        url <- paste0(sub("/[0-9]+$", "/", req$url), n - 1)
      }
      res$redirect(url, 302L)
    }
  })

  app$get(c("/redirect/:n", "/relative-redirect/:n"), function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    if (is.na(n)) {
      res$send_status(404L)
    } else {
      if (n == 1) {
        url <- sub("/redirect/[0-9]+$", "/get", req$path)
        url <- sub("/relative-redirect/[0-9]+$", "/get", req$path)
      } else {
        n <- min(n, 5)
        url <- paste0(sub("/[0-9]+$", "/", req$path), n - 1)
      }
      res$redirect(url, 302L)
    }
  })

  app$all("/redirect-to", function(req, res) {
    res$redirect(req$query$url, req$query$status_code %||% 302)
  })

  # Anything =============================================================

  app$all(new_regexp("^/anything"), common_response)

  app
}
