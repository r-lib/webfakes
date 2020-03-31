
library(pressr)

app <- new_app()
app$use(mdd_log())

app$use(mdd_static("public"))

app$listen(as.integer(Sys.getenv("PORT", NA_character_)))
