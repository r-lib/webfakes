
#' @export

mdd_static <- function(root, set_headers = NULL) {

  root; set_headers
  function(req, res) {
    path <- file.path(root, sub("^/", "", req$path))
    if (!file.exists(path)) return("next")
    if (file.info(path)$isdir) return("next")
    res$.body <- c(file = normalizePath(path))
    ext <- tools::file_ext(basename(path))
    ct <- mime_find(ext)
    if (!is.na(ct)) {
      res$set_header("content-type", ct)
    }
    if (!is.null(set_headers)) set_headers(req, res)
  }
}
