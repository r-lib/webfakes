
server_start <- function(options = NULL) {
  .Call(c_server_start, options %||% character())
}

server_handler <- function(rawreq) {
  message("handling!")
}
