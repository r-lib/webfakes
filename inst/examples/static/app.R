
library(webfakes)

app <- new_app()
app$use(mw_log())

app$use(mw_static("public"))

app$listen(as.integer(Sys.getenv("PORT", NA_character_)))
