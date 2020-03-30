
library(pressr)
library(jsonlite)

app <- new_app()
app$use(mdd_log())
app$use(mdd_json())
app$use(mdd_text(type = c("text/plain", "application/json")))

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

app$all(
  new_regexp("^/status/(?<status>[0-9][0-9][0-9])$"),
  function(req, res) {
    res$send_status(req$params$status)
  }
)

app$all("/redirect-to", function(req, res) {
  res$redirect(req$query$url, req$query$status_code %||% 302)
})

app$listen(as.integer(Sys.getenv("PORT", NA_character_)))
