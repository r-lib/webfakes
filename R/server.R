
server_start <- function(port = NULL) {
  options <- c(
    "listening_ports"    = as.character(port %||% "0"),
    "request_timeout_ms" = "100000",
    "num_threads"        = "1"
  )
  .Call(c_server_start, options)
}

server_get_ports <- function(srv) {
  as.data.frame(.Call(c_server_get_ports, srv))
}

server_process <- function(srv, handler) {
  invisible(.Call(c_server_process, srv, handler, environment()))
}

server_stop <- function(srv) {
  invisible(.Call(c_server_stop, srv))
}
