
test_response_app <- function() {
  app <- new_app()

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
      root = system.file(package = "presser"),
      file.path("examples", "static", "public", "foo", "bar.json")
    )
  })

  app$get("/type", function(req, res) {
    res$set_type("json")
    res$send("{ \"foo\": 1 }")
  })

  app$get("/write", function(req, res) {
    res$
      write("hello ")$
      write("world!")
  })

  app$get("/write-header", function(req, res) {
    res$
      set_header("foo", "bar")$
      write("hello ")$
      write("world!")
  })

  app$get("/write-wait", function(req, res) {
    res$locals$turn <- (res$locals$turn %||% 0) + 1L
    if (res$locals$turn == 1) {
      res$
        set_header("content-length", nchar("hello world!"))$
        write("hell")$
        delay(0.01)
    } else if (res$locals$turn == 2) {
      res$
        write("o world")$
        delay(0.01)
    } else {
      res$send("!")
    }
  })

  app$get("/send-chunk", function(req, res) {
    res$locals$turn <- (res$locals$turn %||% 0) + 1L
    if (res$locals$turn == 1) {
      res$
        set_header("Content-Type", "text/plain")$
        send_chunk("first chunk\n")$
        delay(0.01)
    } else if (res$locals$turn == 2) {
      res$
        send_chunk("second chunk\n")$
        delay(0.01)
    } else {
      res$send_chunk("third and final chunk\n")
    }
  })

  app
}
