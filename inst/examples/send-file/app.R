
library(webfakes)

app <- new_app()
app$use(mw_log())
app$use(mw_etag())

app$get("/logo", function(req, res) {
  res$send_file("Rlogo.png")
})

app$listen(3000L)
