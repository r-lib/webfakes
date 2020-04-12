
#' Generic web app for testing HTTP clients
#'
#' A web app similar to <https://httpbin.org>.
#'
#' @param log Whether to log requests to the standard output.
#' @return A `presser_app`.
#'
#' @export
#' @examples
#' app <- httpbin_app()
#' proc <- new_app_process(app)
#' url <- proc$url("/get")
#' resp <- curl::curl_fetch_memory(url)
#' curl::parse_headers_list(resp$headers)
#' cat(rawToChar(resp$content))
#' proc$stop()

httpbin_app <- function(log = interactive()) {

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
  if (log) app$use("logger" = mw_log())

  # Parse all kinds of bodies
  app$use("json body parser" = mw_json())
  app$use("text body parser" = mw_text(type = c("text/plain", "application/json")))
  app$use("multipart body parser" = mw_multipart())
  app$use("URL encoded body parser" = mw_urlencoded())

  # Add etags by default
  app$use("add etag" = mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", as.character(Sys.time()))
    "next"
  })

  make_common_response <- function(req, res) {
    ret <- list(
      args = as.list(req$query),
      data = req$text,
      files = encode_files(req$files),
      form = req$form,
      headers = req$headers,
      json = req$json,
      method = req$method,
      path = req$path,
      origin = req$remote_addr,
      url = req$url
    )
  }

  common_response <- function(req, res) {
    ret <- make_common_response(req, res)
    res$send_json(object = ret, auto_unbox = TRUE, pretty = TRUE)
  }

  # Main page, this will be the documentation of the API, eventually

  app$get("/", function(req, res) {
    res$send_file(
      root = system.file(package = "presser", "examples", "httpbin", "data"),
      "index.html"
    )
  })

  # HTTP methods =========================================================

  common_get <- function(req, res) {
    ret <- list(
      args = as.list(req$query),
      headers = req$headers,
      origin = req$remote_addr,
      path = req$path,
      url = req$url
    )
    res$send_json(object = ret, auto_unbox = TRUE, pretty = TRUE)
  }

  app$get("/get", common_get)

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
    hdr <- req$get_header("Authorization") %||% ""
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
    auth <- req$get_header("Authorization") %||% ""
    if (! grepl("^Bearer ", auth)) {
      res$
        set_header("WWW-Authenticate", "bearer")$
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
    ret <- list("user-agent" = req$get_header("User-Agent"))
    res$send_json(ret, auto_unbox = TRUE, pretty = TRUE)
  })

  # Response inspection ==================================================

  app$get("/etag/:etag", function(req, res) {
    etag <- req$params$etag

    # The mw_etag() middleware is active, so we need to do this after that
    res_etag <- NULL
    res$on_response(function(req, res) {
      if (!is.null(res_etag)) res$set_header("Etag", res_etag)
    })

    parse <- function(x) {
      x <- strsplit(x, ",", fixed = TRUE)[[1]]
      re_match(x, '\\s*(W/)?"?([^"]*)"?\\s*')$groups[,2]
    }

    if_none_match <- parse(req$get_header("If-None-Match") %||% "")
    if_match <- parse(req$get_header("If-Match") %||% "")

    if (length(if_none_match) > 0) {
      if (etag %in% if_none_match || "*" %in% if_none_match) {
        res$set_status(304)
        res_etag <- "etag"
        return()
      }
    } else if (length(if_match) > 0) {
      if ((! etag %in% if_match) && (!"*" %in% if_match)) {
        res$set_status(412)
        return()
      }
    }

    res_etag <- etag
    common_get(req, res)
  })

  # TODO: /cache * /cache/{value} * /etag * /response-headers (2x)

  # Response formats =====================================================

  app$get("/deny", function(req, res) {
    res$
      set_type("text/plain")$
      send_file(
        root = system.file(package = "presser"),
        file.path("examples", "httpbin", "data", "deny.txt")
      )
  })

  app$get("/gzip", function(req, res) {
    ret <- make_common_response(req, res)
    json <- jsonlite::toJSON(ret, auto_unbox = TRUE, pretty = TRUE)
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    con <- file(tmp, open = "wb")
    con2 <- gzcon(con)
    writeBin(charToRaw(json), con2)
    flush(con2)
    close(con2)
    gzipped <- readBin(tmp, "raw", file.info(tmp)$size)
    res$
      set_type("application/json")$
      set_header("Content-Encoding", "gzip")$
      send(gzipped)
  })

  app$get("/encoding/utf8", function(req, res) {
    res$
      set_type("text/html; charset=utf-8")$
      send_file(
        root = system.file(package = "presser"),
        file.path("examples", "httpbin", "data", "utf8.html")
      )
  })

  app$get("/html", function(req, res) {
    res$send_file(
      root = system.file(package = "presser"),
      file.path("examples", "httpbin", "data", "example.html")
    )
  })

  app$get("/json", function(req, res) {
    res$send_file(
      root = system.file(package = "presser"),
      file.path("examples", "httpbin", "data", "example.json")
    )
  })

  app$get("/robots.txt", function(req, res) {
    res$send_file(
      root = system.file(package = "presser"),
      file.path("examples", "httpbin", "data", "robots.txt")
    )
  })

  app$get("/xml", function(req, res) {
    res$send_file(
      root = system.file(package = "presser"),
      file.path("examples", "httpbin", "data", "example.xml")
    )
  })

  # TODO: /brotli * /deflate

  # Dynamic data =========================================================

  app$get(list("/base64", new_regexp("/base64/(?<value>[\\+/=a-zA-Z0-9]*)")),
          function(req, res) {
    value <- req$params$value %||% ""
    if (value == "") value <- "RXZlcnl0aGluZyBpcyBSc29tZQ=="
    plain <- charToRaw(base64_decode(value))
    res$
      set_type("application/octet-stream")$
      send(plain)
  })

  app$get("/bytes/:n", function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    if (is.na(n)) {
      return("next")
    } else {
      n <- min(n, 10000)
      bytes <- as.raw(as.integer(floor(stats::runif(n, min=0, max=256))))
      res$
        set_type("application/octet-stream")$
        send(bytes)
    }
  })

  app$all(new_regexp("/delay/(?<delay>[0-9\\.]+)$"), function(req, res) {
    delay <- suppressWarnings(as.numeric(req$params$delay))
    if (is.na(delay)) {
      return("next")
    } else if (is.null(res$locals$seen)) {
      res$locals$seen <- TRUE
      delay <- min(delay, 10)
      res$delay(delay)
    } else if (req$method == "head") {
      res$send_status(200L)
    } else {
      common_response(req, res)
    }
  })

  app$get("/uuid", function(req, res) {
    ret <- list(uuid = uuid::UUIDgenerate())
    res$send_json(ret, auto_unbox = TRUE, pretty = TRUE)
  })

  # TODO: /drip *
  # /links/{n}{offset} * /range/{numbytes} * /stream-bytes/{n} *
  # /stream/{n}

  # Cookies ==============================================================

  # TODO: /cookies * /cookies/delete * /cookies/set *
  # /cookies/set/{name}/{value}

  # Images ===============================================================

  app$get("/image", function(req, res) {
    act <- req$get_header("Accept")
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
      res$send_file(
        root = system.file(package = "presser"),
        file.path("examples", "httpbin", "images", fls[act])
      )
    }
  })

  app$get(new_regexp("/image/(?<format>jpeg|png|svg|webp)"),
          function(req, res) {
    filename <- paste0("Rlogo.", req$params$format)
    res$send_file(
      root = system.file(package = "presser"),
      file.path("examples", "httpbin", "images", filename)
    )
  })

  # Redirects ============================================================

  app$get("/absolute-redirect/:n", function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    if (is.na(n)) {
      return("next")
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
      return("next")
    } else {
      if (n == 1) {
        url <- sub("/redirect/[0-9]+$", "/get", req$path)
        url <- sub("/relative-redirect/[0-9]+$", "/get", url)
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
