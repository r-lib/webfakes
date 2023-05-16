
#' Generic web app for testing HTTP clients
#'
#' A web app similar to <https://httpbin.org>.
#' See [its specific docs](https://webfakes.r-lib.org/httpbin.html).
#' You can also see these docs locally, by starting the app:
#' ```r
#' httpbin <- new_app_process(httpbin_app())
#' browseURL(httpbin$url())
#' ```
#'
#' @param log Whether to log requests to the standard output.
#' @return A `webfakes_app`.
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
  app$use("cookie parser" = mw_cookie_parser())

  # Add etags by default
  app$use("add etag" = mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", http_time_stamp())
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

  # Main page

  app$get("/", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes", "examples", "httpbin", "assets"),
      "httpbin.html"
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

  app$get("/forms/post", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes", "examples", "httpbin", "assets"),
      "forms-post.html"
    )
  })

  # Auth =================================================================

  basic_auth <- function(req, res, error_status = 401L) {
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
      if (error_status == 401L) {
        res$
          set_header("WWW-Authenticate", "Basic realm=\"Fake Realm\"")$
          send_status(error_status)
      } else {
        res$
          send_status(error_status)
      }
    }
  }

  app$get("/basic-auth/:user/:passwd", function(req, res) {
    basic_auth(req, res, error_status = 401L)
  })

  app$get("/hidden-basic-auth/:user/:passwd", function(req, res) {
    basic_auth(req, res, error_status = 404L)
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

  hash <- function(str, algorithm) {
    algo <- tolower(algorithm %||% "md5")
    algo <- c("md5" = "md5", "sha-256" = "sha256", "sha-512" = "sha512")[algo]
    if (is.na(algo)) {
      stop("Unknown hash algorithm for digest auth: ", algorithm)
    }
    digest::digest(str, algo = algo, serialize = FALSE)
  }

  hash1 <- function(realm, username, password, algorithm) {
    realm <- realm %||% ""
    hash(paste(collapse = ":", c(
      username,
      realm,
      password
    )), algorithm)
  }

  hash2 <- function(credentials, req, algorithm) {
    qop <- credentials[["qop"]] %||% "auth"
    query <- if (nchar(req$query_string %||% "")) paste0("?", req$query_string)
    req_uri <- paste0(req$path, query)
    if (qop == "auth") {
      hash(paste(collapse = ":", c(
        toupper(req[["method"]]),
        req_uri
      )), algorithm)

    } else {
      hash(paste0(collapse = ":", c(
        toupper(req[["method"]]),
        req_uri,
        hash(req$.body %||% "", algorithm)
      )), algorithm)
    }
  }

  digest_challenge_response <- function(req, qop, algorithm, stale = FALSE) {
    nonce <- hash(
      paste0(req$remote_addr, ":", unclass(Sys.time()), ":", random_id(10)),
      algorithm
    )

    opaque <- hash(random_id(10), algorithm)
    realm <- "webfakes.r-lib.org"
    qop <- qop %||% "auth,auth-int"

    wwwauth <- paste0(
      "Digest ",
      "realm=\"", realm, "\", ",
      "qop=\"", qop, "\", ",
      "nonce=\"", nonce, "\", ",
      "opaque=\"", opaque, "\", ",
      "algorithm=", algorithm, ", ",
      "stale=", stale
    )

    wwwauth
  }

  next_stale_after_value <- function(x) {
    x <- suppressWarnings(as.integer(x))
    if (is.na(x)) "never" else as.character(x - 1L)
  }

  check_digest_auth <- function(req, credentials, user, passwd) {
    if (is.null(credentials)) return(FALSE)
    algorithm <- credentials[["algorithm"]]
    HA1_value <- hash1(
      credentials[["realm"]],
      credentials[["username"]],
      passwd,
      algorithm
    )
    HA2_value <- hash2(credentials, req, algorithm)

    qop <- credentials[["qop"]] %||% "compat"
    if (! qop %in% c("compat", "auth", "auth-int")) return(FALSE)

    response_hash <- if (qop == "compat") {
      hash(paste0(c(
        HA1_value,
        credentials[["nonce"]] %||% "",
        HA2_value
      ), collapse = ":"), algorithm)

    } else {
      if (any(! c("nonce", "nc", "cnonce", "qop") %in% names(credentials))) {
        return(FALSE)
      }
      hash(paste0(c(
        HA1_value,
        credentials[["nonce"]],
        credentials[["nc"]],
        credentials[["cnonce"]],
        credentials[["qop"]],
        HA2_value
      ), collapse = ":"), algorithm)
    }

    (credentials[["response"]] %||% "") == response_hash
  }

  digest_auth <- function(req, res, qop, user, passwd, algorithm, stale_after) {
    require_cookie_handling <-
      tolower(req$query$`require-cookie` %||% "") %in% c("1", "t", "true")
    if (! algorithm %in% c("MD5", "SHA-256", "SHA-512")) {
      algorithm <- "MD5"
    }

    if (! qop %in% c("auth", "auth-int")) {
      qop <- NULL
    }

    authorization <- req$get_header("Authorization")
    credentials <- if (!is.null(authorization)) {
      parse_authorization_header(authorization)
    }

    if (is.null(authorization) ||
        is.null(credentials) ||
        tolower(credentials$scheme) != "digest" ||
        (require_cookie_handling && is.null(req$get_header("Cookie")))
        ) {
      wwwauth <- digest_challenge_response(req, qop, algorithm)
      res$
        set_status(401L)$
        add_cookie("stale_after", stale_after)$
        add_cookie("fake", "fake_value")$
        set_header("WWW-Authenticate", wwwauth)$
        send("")
      return()
    }

    if (require_cookie_handling &&
        (req$cookies[["fake"]] %||% "") != "fake_value") {
      res$
        add_cookie("fake", "fake_value")$
        add_status(403L)$
        send_json(
          list(errors = "missing cookie set on challenge"),
          pretty = TRUE
        )
      return()
    }

    current_nonce <- credentials$nonce
    stale_after_value <- req$cookies$stale_after %||% ""
    if (identical(current_nonce, req$cookies[["last_nonce"]] %||% "") ||
        stale_after_value == "0") {
      wwwauth <- digest_challenge_response(req, qop, algorithm, TRUE)
      res$
        set_status(401L)$
        add_cookie("stale_after", stale_after)$
        add_cookie("last_nonce", current_nonce)$
        add_cookie("fake", "fake_value")$
        set_header("WWW-Authenticate", wwwauth)$
        send("")
      return()
    }

    if (!check_digest_auth(req, credentials, user, passwd)) {
      wwwauth <- digest_challenge_response(req, qop, algorithm, FALSE)
      res$
        set_status(401L)$
        add_cookie("stale_after", stale_after)$
        add_cookie("last_nonce", current_nonce)$
        add_cookie("fake", "fake_value")$
        set_header("WWW-Authenticate", wwwauth)$
        send("")
      return()
    }

    res$add_cookie("fake", "fake_value")
    if (!is.null(stale_after_value)) {
      res$add_cookie(
        "stale_after",
        next_stale_after_value(stale_after_value)
      )
    }

    res$
      send_json(
        list(authentication = TRUE, user = user),
        pretty = TRUE,
        auto_unbox = TRUE
      )
  }

  app$get("/digest-auth/:qop/:user/:passwd", function(req, res) {
    qop <- req$params$qop
    user <- req$params$user
    passwd <- req$params$passwd
    digest_auth(req, res, qop, user, passwd, "MD5", "never")
  })

  app$get("/digest-auth/:qop/:user/:passwd/:algorithm", function(req, res) {
    qop <- req$params$qop
    user <- req$params$user
    passwd <- req$params$passwd
    algorithm <- req$params$algorithm
    digest_auth(req, res, qop, user, passwd, algorithm, "never")
  })

  app$get(
    "/digest-auth/:qop/:user/:passwd/:algorithm/:stale_after",
    function(req, res) {
      qop <- req$params$qop
      user <- req$params$user
      passwd <- req$params$passwd
      algorithm <- req$params$algorithm
      stale_after <- req$params$stale_after
      digest_auth(req, res, qop, user, passwd, algorithm, stale_after)
    }
  )

  # Status codes =========================================================

  app$all(
    new_regexp("^/status/(?<status>[0-9][0-9][0-9])$"),
    function(req, res) {
      status <- req$params$status
      res$set_status(status)
      if (status == "418") {
        res$send(paste(
          sep = "\n",
          "",
          "    -=[ teapot ]=-",
          "",
          "       _...._",
          "     .'  _ _ `.",
          "    | .\"` ^ `\". _,",
          "    \\_;`\"---\"`|//",
          "      |       ;/",
          "      \\_     _/",
          "        `\"\"\"`",
          ""
        ))
      } else {
        res$send("")
      }
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
      if (!is.null(res_etag)) res$set_header("ETag", res_etag)
    })

    parse <- function(x) {
      x <- strsplit(x, ",", fixed = TRUE)[[1]]
      re_match(x, '\\s*(W/)?"?([^"]*)"?\\s*')$groups[,2]
    }

    if_none_match <- parse(req$get_header("If-None-Match") %||% "")
    if_match <- parse(req$get_header("If-Match") %||% "")

    if (length(if_none_match) > 0) {
      if (etag %in% if_none_match || "*" %in% if_none_match) {
        res$send_status(304)
        res_etag <- "etag"
        return()
      }
    } else if (length(if_match) > 0) {
      if ((! etag %in% if_match) && (!"*" %in% if_match)) {
        res$send_status(412)
        return()
      }
    }

    res_etag <- etag
    common_get(req, res)
  })

  rsp_hdrs <- function(req, res) {

    obj <- structure(list(), names = character())
    for (i in seq_along(req$query)) {
      key <- names(req$query)[i]
      res$add_header(key, req$query[[i]])
      obj[[key]] <- c(obj[[key]], req$query[[i]])
    }

    res$send_json(object = obj, auto_unbox = TRUE)
  }
  app$get("/response-headers", rsp_hdrs)
  app$post("/response-headers", rsp_hdrs)

  app$get("/cache", function(req, res) {
    if (is.null(req$get_header("If-Modified-Since")) &&
        is.null(req$get_header("If-None-Match"))) {
      res$set_header("Last-Modified", http_time_stamp())
      # etag is added by default
      common_response(req, res)
    } else {
      res$send_status(304)
    }
  })

  app$get("/cache/:value", function(req, res) {
    value <- suppressWarnings(as.integer(req$params$value))
    if (is.na(value)) {
      "next"
    } else {
      res$set_header(
            "Cache-Control",
            sprintf("public, max-age=%d", value)
          )
      common_response(req, res)
    }
  })

  # Response formats =====================================================

  app$get("/deny", function(req, res) {
    res$
      set_type("text/plain")$
      send_file(
        root = system.file(package = "webfakes"),
        file.path("examples", "httpbin", "data", "deny.txt")
      )
  })

  app$get("/brotli", function(req, res) {
    ret <- make_common_response(req, res)
    ret$brotli <- TRUE
    json <- jsonlite::toJSON(ret, auto_unbox = TRUE, pretty = TRUE)
    data <- charToRaw(json)
    datax <- brotli::brotli_compress(data)
    res$
      set_type("application/json")$
      set_header("Content-Encoding", "brotli")$
      send(datax)
  })

  app$get("/gzip", function(req, res) {
    ret <- make_common_response(req, res)
    ret$gzipped <- TRUE
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

  app$get("/deflate", function(req, res) {
    ret <- make_common_response(req, res)
    ret$deflated <- TRUE
    json <- jsonlite::toJSON(ret, auto_unbox = TRUE, pretty = TRUE)
    data <- charToRaw(json)
    datax <- zip::deflate(data)
    res$
      set_type("application/json")$
      set_header("Content-Encoding", "deflate")$
      send(datax$output)
  })

  app$get("/encoding/utf8", function(req, res) {
    res$
      set_type("text/html; charset=utf-8")$
      send_file(
        root = system.file(package = "webfakes"),
        file.path("examples", "httpbin", "data", "utf8.html")
      )
  })

  app$get("/html", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes"),
      file.path("examples", "httpbin", "data", "example.html")
    )
  })

  app$get("/json", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes"),
      file.path("examples", "httpbin", "data", "example.json")
    )
  })

  app$get("/robots.txt", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes"),
      file.path("examples", "httpbin", "data", "robots.txt")
    )
  })

  app$get("/xml", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes"),
      file.path("examples", "httpbin", "data", "example.xml")
    )
  })

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

  app$get("/drip", function(req, res) {
    # First time?
    if (is.null(res$locals$drip)) {
      duration <- as.double(req$query$duration %||% 2)
      numbytes <- as.integer(req$query$numbytes %||% 10)
      code <- as.integer(req$query$code %||% 200L)
      delay <- as.double(req$query$delay %||% 0)
      # how much to wait between messages, at least 10ms
      pause <- max(duration / numbytes, 0.01)
      # how many messages
      nummsg <- duration / pause + 1
      # how big is a message, at least a byte
      msgsize <- max(floor(numbytes / nummsg), 1)
      res$locals$drip <- list(
        tosend = numbytes,
        msgsize = msgsize,
        pause = pause
      )

      res$
        set_header("Content-Length", numbytes)$
        set_header("Content-Type", "application/octet-stream")$
        set_status(code)

      if (delay > 0) return(res$delay(delay))
    }

    len <- min(res$locals$drip$tosend, res$locals$drip$msgsize)
    res$write(strrep("*", len))
    res$locals$drip$tosend <- res$locals$drip$tosend - len
    if (res$locals$drip$tosend == 0) {
      res$send("")
    } else {
      res$delay(res$locals$drip$pause)
    }
  })

  app$get(new_regexp("^/stream/(?<n>[0-9]+)$"), function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    n <- min(n, 100)
    if (length(n) == 0 || is.na(n)) return("next")
    msg <- make_common_response(req, res)[c("url", "args", "headers", "origin")]

    res$set_type("application/json")

    for (i in seq_len(n)) {
      msg$id <- i - 1L
      txt <- paste0(jsonlite::toJSON(msg, auto_unbox = TRUE), "\n")
      res$send_chunk(charToRaw(txt))
    }
  })

  app$get(new_regexp("^/stream-bytes/(?<n>[0-9]+)$"), function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    n <- min(n, 100 * 1024)
    seed <- suppressWarnings(as.integer(req$query$seed %||% 42))
    chunk_size <- suppressWarnings(as.integer(req$query$chunk_size %||% 10240))
    if (length(n) == 0 || is.na(n) || length(seed) == 0 || is.na(seed) ||
        length(chunk_size) == 0 || is.na(chunk_size)) return("next")
    oldseed <- .GlobalEnv$.Random.seed
    on.exit(set.seed(oldseed))
    set.seed(seed)
    bytes <- as.raw(as.integer(floor(stats::runif(n, min=0, max=256))))
    nc <- ceiling(n / chunk_size)
    for (i in seq_len(nc)) {
      from <- (i-1)*chunk_size + 1
      to <- min(length(bytes), i * chunk_size)
      res$send_chunk(bytes[from:to])
    }
  })

  re_range <- new_regexp("^/range/(?<numbytes>[0-9]+)$")

  # This is not in httpbin, but it is handy to get the size of the
  # response, and to see whether the server supports ranges

  app$head(re_range, function(req, res) {
    numbytes <- suppressWarnings(as.integer(req$params$n))
    if (length(numbytes) == 0 || is.na(numbytes)) {
      return("next")
    }

    res$
      set_header("ETag", paste0("range", numbytes))$
      set_header("Accept-Ranges", "bytes")$
      set_header("Content-Length", numbytes)$
      send_status(200L)
  })

  app$get(re_range, function(req, res) {
    if (is.null(res$locals$range)) {
      numbytes <- suppressWarnings(as.integer(req$params$n))
      if (length(numbytes) == 0 || is.na(numbytes)) {
        return("next")
      }

      if (numbytes < 0 || numbytes > 100 * 1024) {
        res$
          set_header("ETag", paste0("range", numbytes))$
          set_header("Accept-Ranges", "bytes")$
          set_status(404L)$
          send("number of bytes must be in the range (0, 102400].")
        return()
      }

      chunk_size <- max(1, as.integer(req$query$chunk_size %||% (10 * 1024)))
      duration <- as.integer(req$query$duration %||% 0)
      pause_per_byte <- duration / numbytes
      if (duration == 0) {
        chunk_size <- numbytes
      }

      ranges <- parse_range(req$get_header("Range"))

      # just like httpbin, we do not support multiple ranges
      if (NROW(ranges) != 1) {
        ranges <- NULL
      }

      # This is not exactly the same as httpbin, but rather follows
      # https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests
      # and also how web servers seem to behave.
      #
      # In particular, in these cases we return the full response:
      # - no Range header,
      # - invalid Range header syntax,
      # - overlapping Range header ranges
      #
      # Otherwise, if a range is outside of the size of the response, we
      # return a 416 response.

      if (!is.null(ranges)) {
        ranges[ranges == Inf] <- numbytes
        if (any(ranges[,2] >= numbytes)) {
          res$
          set_header("ETag", paste0("range", numbytes))$
          set_header("Accept-Ranges", "bytes")$
          set_header("Content-Range", paste0("bytes */", numbytes))$
          set_header("Content-Length", 0L)$
          send_status(416L)
          return()
        }
      }

      # we need the response for sure
      abc <- paste(letters, collapse = "")
      bytes <- substr(strrep(abc, numbytes / nchar(abc) + 1), 1, numbytes)

      res$locals$range <- list(
        bytes = bytes,
        chunk_size = chunk_size,
        pause_per_byte = pause_per_byte
      )

      # First part, so send status and headers
      if (is.null(ranges)) {
        res$
          set_header("ETag", paste0("range", numbytes))$
          set_header("Accept-Ranges", "bytes")$
          set_header("Content-Length", numbytes)$
          set_status(200L)

      } else if (nrow(ranges) == 1) {
        # A single range
        res$
          set_header("ETag", paste0("range", numbytes))$
          set_header("Accept-Ranges", "bytes")$
          set_header(
            "Content-Range",
            sprintf("bytes=%d-%d/%d", ranges[1, 1], ranges[1, 2], numbytes)
          )$
          set_header("Content-Length", ranges[1, 2] - ranges[1, 1] + 1L)$
          set_status(206L)

        # This is all we need to send
        res$locals$range$bytes <- substr(
          bytes,
          ranges[1, 1] + 1,
          ranges[1, 2] + 1L
        )

      } else {
        # This cannot happen now, we do not support multiple ranges
        # Maybe later
      }
    }

    # send a part
    chunk_size <- res$locals$range$chunk_size
    pause <- res$locals$range$pause_per_byte

    tosend <- substr(res$locals$range$bytes, 1, chunk_size)
    res$locals$range$bytes <- substr(
      res$locals$range$bytes,
      chunk_size + 1L,
      nchar(res$locals$range$bytes)
    )
    res$write(tosend)
    if (pause * nchar(tosend) > 0) {
      res$delay(pause * nchar(tosend))
    }
  })

  app$get("/uuid", function(req, res) {
    ret <- list(uuid = uuid_random())
    res$send_json(ret, auto_unbox = TRUE, pretty = TRUE)
  })

  app$get(new_regexp("^/links/(?<n>[0-9]+)(/(?<offset>[0-9]+))?$"),
          function(req, res) {
    n <- suppressWarnings(as.integer(req$params$n))
    o <- suppressWarnings(as.integer(req$params$offset))
    if (length(o) == 0 || is.na(o)) o <- 1
    if (length(n) == 0 || is.na(n)) return("next")
    n <- min(max(1, n), 200)
    o <- min(max(1, o), n)
    links <- sprintf("<a href = \"/links/%d/%d\">%d</a>", n, 1:n, 1:n)
    links[o] <- o
    html <- paste0(
      "<html><head><title>Links</title></head><body>",
      paste(links, collapse = " "),
      "</body></html>"
    )
    res$
      set_type("html")$
      send(html)
  })

  # Cookies ==============================================================

  app$get("/cookies", function(req, res) {
    cks <- req$cookies
    res$send_json(
      object = list(cookies = cks),
      auto_unbox = TRUE,
      pretty = TRUE
    )
  })

  app$get("/cookies/set/:name/:value", function(req, res) {
    res$add_cookie(req$params$name, req$params$value)
    res$redirect("/cookies", 302L)
  })

  app$get("/cookies/set", function(req, res) {
    for (n in names(req$query)) {
      res$add_cookie(n, req$query[[n]])
    }
    res$redirect("/cookies", 302L)
  })

  app$get("/cookies/delete", function(req, res) {
    for (n in names(req$query)) {
      res$clear_cookie(n)
    }
    res$redirect("/cookies", 302L)
  })

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
        root = system.file(package = "webfakes"),
        file.path("examples", "httpbin", "images", fls[act])
      )
    }
  })

  app$get(new_regexp("/image/(?<format>jpeg|png|svg|webp)"),
          function(req, res) {
    filename <- paste0("Rlogo.", req$params$format)
    res$send_file(
      root = system.file(package = "webfakes"),
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
