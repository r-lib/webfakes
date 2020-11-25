
library(webfakes)

app <- httpbin_app()

app$listen(as.integer(Sys.getenv("PORT", NA_character_)))
