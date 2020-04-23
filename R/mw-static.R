
#' Middleware function to serve static files
#'
#' The content type of the response is set automatically from the
#' extension of the file. Note that this is a terminal middleware
#' handler function. If a file is served, then the rest of the handler
#' functions will not be called. If a file was not found, however,
#' the rest of the handlers are still called.
#'
#' @param root Root path of the served files. Everything under this
#' directory is served automatically. Directory lists are not currently
#' supports.
#' @param set_headers Callback function to call before a file is served.
#' @return Handler function.
#'
#' @family middleware
#' @export
#' @examples
#' root <- system.file(package = "presser", "examples", "static", "public")
#' app <- new_app()
#' app$use(mw_static(root = root))
#' app

mw_static <- function(root, set_headers = NULL) {

  root; set_headers
  function(req, res) {
    path <- file.path(root, sub("^/", "", req$path))
    if (!file.exists(path)) return("next")
    if (file.info(path)$isdir) return("next")
    ext <- tools::file_ext(basename(path))
    ct <- mime_find(ext)
    if (!is.na(ct)) {
      res$set_header("Content-Type", ct)
    }
    if (!is.null(set_headers)) set_headers(req, res)
    res$send(read_bin(path))
  }
}
