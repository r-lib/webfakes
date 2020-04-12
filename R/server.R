
server_start <- function(port = NULL, num_threads = 10) {
  port <- paste0("127.0.0.1:", as.character(port %||% "0"))
  options <- c(
    "listening_ports"          = port,
    "request_timeout_ms"       = "100000",
    "num_threads"              = as.character(num_threads),
    "enable_auth_domain_check" = "no"
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
