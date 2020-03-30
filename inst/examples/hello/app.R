
library(pressr)

app <- new_app()
app$engine("txt", tmpl_glue())
app$use(mdd_log())

app$get("/hello", function(req, res) {
  res$send("Hello there!")
})

app$get(new_regexp("^/hi(/.*)?$"), function(req, res) {
  res$send("Hi indeed!")
})

app$post("/hello", function(req, res) {
  res$send("Got it, thanks!")
})

app$get("/view", function(req, res) {
  txt <- res$render("test")
  res$set_header("content-type", "text/plain")
  res$send(txt)
})

app$listen(3000L)
