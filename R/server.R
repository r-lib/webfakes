
server_start <- function(opts = server_opts()) {
  ports <- paste0(opts$interfaces, ":", opts$port %||% "0", collapse = ",")
  options <- c(
    "listening_ports"          = ports,
    "num_threads"              = opts$num_threads,
    "request_timeout_ms"       = "100000",
    "enable_auth_domain_check" = "no"
  )
  .Call(c_server_start, options)
}

#' Presser web server options
#'
#' @param port Port to start the web server on. Defaults to a randomly
#'   chosen port.
#' @param num_threads Number of request handler threads to use. Typically
#'   you don't need more than one thread, unless you run test cases in
#'   parallel or you make concurrent HTTP requests.
#' @param interfaces The network interfaces to listen on. Being a test
#'   web server, it defaults to the localhost. Only bind to a public
#'   interface if you know what you are doing. presser was not designed
#'   to serve public web pages.
#' @export

server_opts <- function(port = NULL, num_threads = 1,
                        interfaces = "127.0.0.1") {
  list(
    port = port,
    num_threads = num_threads,
    interfaces = interfaces
  )
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
