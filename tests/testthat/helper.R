test_response_app <- function() {
  app <- new_app()

  `%||%` <- function(l, r) if (is.null(l)) r else l

  app$locals$applocal <- "foo"

  app$engine("txt", tmpl_glue())
  app$get("/local", function(req, res) {
    res$locals$reslocal <- "bar"
    "next"
  })

  app$get("/local", function(req, res) {
    res$send(paste(res$locals$applocal, res$locals$reslocal))
  })

  app$get("/badengine", function(req, res) {
    txt <- res$render("foobar")
    res$send(txt)
  })

  app$get("/badjson", function(req, res) {
    res$send_json(1:3, text = "foo")
  })

  app$get("/file", function(req, res) {
    res$send_file(
      root = system.file(package = "webfakes"),
      file.path("examples", "static", "public", "foo", "bar.json")
    )
  })

  app$get("/type", function(req, res) {
    res$set_type("json")
    res$send("{ \"foo\": 1 }")
  })

  app$get("/write", function(req, res) {
    res$write("hello ")$write("world!")
  })

  app$get("/write-header", function(req, res) {
    res$set_header("foo", "bar")$write("hello ")$write("world!")
  })

  app$get("/write-wait", function(req, res) {
    res$locals$turn <- (res$locals$turn %||% 0) + 1L
    if (res$locals$turn == 1) {
      res$set_header("content-length", nchar("hello world!"))$write(
        "hell"
      )$delay(0.01)
    } else if (res$locals$turn == 2) {
      res$write("o world")$delay(0.01)
    } else {
      res$send("!")
    }
  })

  app$get("/send-chunk", function(req, res) {
    res$locals$turn <- (res$locals$turn %||% 0) + 1L
    if (res$locals$turn == 1) {
      res$set_header("Content-Type", "text/plain")$send_chunk(
        "first chunk\n"
      )$delay(0.01)
    } else if (res$locals$turn == 2) {
      res$send_chunk("second chunk\n")$delay(0.01)
    } else {
      res$send_chunk("third and final chunk\n")
    }
  })

  app$get("/add-header", function(req, res) {
    res$add_header("foo", "bar")
    res$add_header("foo", "bar2")
    res$add_header("foobar", "baz")
    res$send("ready")
  })

  app
}

httpbin <- local_app_process(httpbin_app(), opts = server_opts(num_threads = 6))
httpbin$local_env(c(FOO = "{url}xxx"))

r_variant <- function() {
  if (getRversion() < "4.2.0") {
    "old-r"
  } else {
    "new-r"
  }
}

callr_curl <- function(url, options = list()) {
  callr::r(
    function(url, options) {
      h <- curl::new_handle()
      curl::handle_setopt(h, .list = options)
      curl::curl_fetch_memory(url, handle = h)
    },
    list(url = url, options = options),
    env = c(
      callr::rcmd_safe_env(),
      CURL_SSL_BACKEND = "openssl",
      CURL_CA_BUNDLE = if ("cainfo" %in% names(options)) options$cainfo
    )
  )
}
