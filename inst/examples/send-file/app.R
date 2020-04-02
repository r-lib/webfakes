
library(presser)

app <- new_app()
app$use(mdd_log())
app$use(mdd_etag())

app$get("/logo", function(req, res) {
  res$send_file("Rlogo.png")
})

app$listen(3000L)
