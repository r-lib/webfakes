
server_start <- function(options = NULL) {
  .Call(c_server_start, options %||% character())
}

server_process <- function(srv, handler) {
  .Call(c_server_process, srv, handler, environment())
}

server_handler <- function(rawreq) {
  message("handling!")
}
