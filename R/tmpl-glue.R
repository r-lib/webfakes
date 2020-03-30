
#' @export

tmpl_glue <- function(sep = "", open = "{", close = "}", na = "NA",
                      transformer = NULL, trim = TRUE) {
  sep; open; close; na; transformer; trim
  function(path, locals) {
    txt <- readChar(path, nchars = file.size(path), useBytes = TRUE)
    glue::glue_data(
      locals, txt, .sep = sep, .open = open, .close = close, .na = na,
      .transformer = transformer %||% glue::identity_transformer,
      .trim = trim
    )
  }
}
