
oauth2_resource_app <- function(redirect_uri) {

  # Create app
  app <- new_app()

  # Parse body for /token
  app$use(mw_urlencoded())
  app$use(mw_json())

  app$locals$tpapps <- data.frame(
    name = "Third-Party Application",
    client_secret = "client_secret",
    client_id = "client_id",
    redirect_uri = redirect_uri
  )

  generate_token <- function() {
    paste0(sample(c(0:9, letters[1:6]), 30, replace = TRUE), collapse = "")
  }

  app$set_config("views", system.file("views", package = "presser"))
  app$engine("html", tmpl_glue())

  # First step, asking the user for access
  app$get("/authorize", function(req, res) {

    # Missing or invalid client id
    if (is.null(req$query$client_id)) {
      res$
        set_status(400L)$
        send("Invalid authorization request, no client id")
      return()

    } else if (! req$query$client_id %in% app$locals$tpapps$client_id) {
      res$
        set_status(400L)$
        send("Invalid authorization request, unknown client id")
      return()
    }

    tpapps <- app$locals$tpapps
    tprec <- tpapps[match(req$query$client_id, tpapps$client_id), ]

    # Bad redirect URL?
    if (req$query$redirect_uri %||% "" != tprec$redirect_uri) {
      res$
        set_status(400L)$
        send("Invalid authorization request, redirect URL mismatch")
      return()
    }

    code <- generate_token()
    # TODO: make this app specific
    app$locals$codes <- c(app$locals$codes, code)

    data <- list(
      app = tprec$name,
      redirect_uri = tprec$redirect_uri,
      code = code,
      state = req$query$state %||% ""
    )
    html <- res$render("authorize", data)
    res$
      set_type("text/html")$
      send(html)
  })

  app$post("/token", function(req, res) {

    if (req$form$grant_type %||% "" != "authorization_code") {
      res$
        set_status(400L)$
        send("Invalid grant type, must be 'authorization_code'")
      return()
    }

    if (! req$form$code %in% app$locals$codes) {
      res$
        set_status(400L)$
        send("Unknown authorization code")
      return()
    }

    tpapps <- app$locals$tpapps

    if (! req$form$client_id %in% tpapps$client_id) {
      res$
        set_status(400L)$
        send("Invalid client id")
      return()
    }

    tprec <- tpapps[match(req$form$client_id, tpapps$client_id), ]

    if (req$form$client_secret %||% "" != tprec$client_secret) {
      res$
        set_status(400L)$
        send("Invalid token request, client secret mismatch")
      return()
    }

    if (req$form$redirect_uri %||% "" != tprec$redirect_uri) {
      res$
        set_status(400L)$
        send("Invalid token request, redirect URL mismatch")
      return()
    }

    token <- paste0("token-", generate_token())
    app$locals$tokens <- c(req$app$locals$tokens, token)
    app$locals$codes <- setdiff(app$locals$codes, req$query$code)

    res$
      send_json(
        list(access_token = token, expires_in = 3600L),
        auto_unbox = TRUE
      )
  })

  app
}

oauth2_third_party_app <- function() {
  app <- new_app()
  app$locals$client_secret <- "client_secret"
  app$locals$client_id <- "client_id"

  app$get("/login", function (req, res) {

    state <- sodium::bin2hex(sodium::random(5))

    # might need a cookie to store the state
    req$app$locals$state <- state

    url <- httr::modify_url(
      app$locals$server_url,
        path = "authorize",
        query = list(
          state = state,
          client_secret = req$app$locals$client_secret,
          client_id = req$app$locals$client_id,
          redirect_uri = paste0(urltools::scheme(req$url), "://", urltools::domain(req$url), ":", urltools::port(req$url))
        )
      )

    res$
      set_header("location", url)$
      send_status(302L)

  })

  app$get("/cb", function (req, res) {

    httr::POST(
      httr::modify_url(
        app$locals$server_url,
        path = "token",
        query = list(
          grant_type="authorization_code",
          code = req$query$code,
          client_id = app$locals$client_id,
          client_secret = app$locals$client_secret
        )
      )
    ) -> resp

    content <- httr::content(resp)

    res$
      set_status(200L)$
      send_json(content)

  })
}

useless <- function() {
  app <- new_app()
  app$locals$killed <- FALSE
  app$get("/kill", function(req, res) {
    req$app$locals$killed <- TRUE

    res$
      set_type("text/plain")$
      send("Return to R!")
  })

  app$get("/killed", function(req, res) {
    res$
      send_json(
        list(req$app$locals$killed))
  })

  return(app)
}
