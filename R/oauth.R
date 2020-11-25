
oauth2_resource_app <- function() {
  app <- new_app()
  app$use(mw_log())

  # Parse body for /authorize/decision, /token
  app$use(mw_urlencoded())
  app$use(mw_json())

  app$locals$tpapps <- data.frame(
    name = character(),
    client_id = character(),
    client_secret = character(),
    redirect_uri = character()
  )

  app$set_config("views", system.file("views", package = "presser"))
  app$engine("html", tmpl_glue())

  app$get("/register", function(req, res) {
    if (is.null(req$query$name) || is.null(req$query$redirect_uri)) {
      res$
        set_status(400L)$
        send("Cannot register without 'name' and 'redirect_uri'")
      return()
    }

    rec <- list(
      name = req$query$name,
      client_id = paste0("id-", generate_token()),
      client_secret = paste0("secret-", generate_token()),
      redirect_uri = req$query$redirect_uri
    )
    app$locals$tpapps <- rbind(app$locals$tpapps, rec)

    res$send_json(rec)
  })

  app$get("/authorize", function(req, res) {

    # Missing or invalid client id
    client_id <- req$query$client_id
    if (is.null(client_id)) {
      res$
        set_status(400L)$
        send("Invalid authorization request, no client id")
      return()

    } else if (! client_id %in% app$locals$tpapps$client_id) {
      res$
        set_status(400L)$
        send("Invalid authorization request, unknown client id")
      return()
    }

    tpapps <- app$locals$tpapps
    tprec <- tpapps[match(client_id, tpapps$client_id), ]

    # Bad redirect URL?
    if (req$query$redirect_uri %||% "" != tprec$redirect_uri) {
      res$
        set_status(400L)$
        send(paste0(
          "Invalid authorization request, redirect URL mismatch: ",
          req$query$redirect_uri %||% "", " vs ", tprec$redirect_uri
        ))
      return()
    }

    state <- req$query$state %||% generate_token()
    app$locals$states <- c(app$locals$states, set_name(client_id, state))

    html <- res$render("authorize", list(state = state, app = tprec$name))
    res$
      set_type("text/html")$
      send(html)
  })

  app$post("/authorize/decision", function(req, res) {
    state <- req$form$state
    if (is.null(state) || ! state %in% names(app$locals$states)) {
      res$
        set_status(400L)$
        send("Invalid decision, no state")
      return()
    }

    client_id <- app$locals$states[state]
    tpapps <- app$locals$tpapps
    tprec <- tpapps[match(client_id, tpapps$client_id), ]

    app$local$states <-
      app$local$states[setdiff(names(app$local$states), state)]

    if (req$form$action %||% "" == "yes") {
      code <- generate_token()
      # TODO: make this app specific
      app$locals$codes <- c(app$locals$codes, code)

      red_uri <- paste0(tprec$redirect_uri, "?code=", code, "&state=", state)
      res$redirect(red_uri)$send()

    } else {
      res$
        send("Maybe next time.")
    }
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
    app$locals$tokens <- c(app$locals$tokens, token)
    app$locals$codes <- setdiff(app$locals$codes, req$query$code)

    res$
      send_json(
        list(access_token = token, expires_in = 3600L),
        auto_unbox = TRUE
      )
  })

  app
}

oauth2_third_party_app <- function(name = "Third-Party app") {
  app <- new_app()
  app$use(mw_log())

  app$use(mw_urlencoded())
  app$use(mw_json())

  app$locals$auth_url <- NA_character_
  app$locals$token_url <- NA_character_
  app$locals$client_id <- NA_character_
  app$locals$client_secret <- NA_character_

  app$post("/login/config", function(req, res) {
    if (is.null(req$json$auth_url) || is.null(req$json$token_url) ||
        is.null(req$json$client_id) || is.null(req$json$client_secret)) {
      res$
        set_status(400L)$
        send("Need `client_id` and `client_secret` to config auth")
      return()
    }

    if (!is.na(app$locals$auth_url)) {
      res$
        set_status(400L)$
        send("Auth already configured")
      return()
    }

    app$locals$auth_url <- req$json$auth_url
    app$locals$token_url <- req$json$token_url
    app$locals$client_id <- req$json$client_id
    app$locals$client_secret <- req$json$client_secret

    res$send_json(list(response = "Authorization configured"))
  })

  app$get("/login", function (req, res) {

    state <- generate_token()
    app$locals$state <- state

    url <- paste0(
      app$locals$auth_url,
      "?client_id=", app$locals$client_id,
      "&redirect_uri=", paste0(req$url, "/redirect"),
      "&state=", state
    )
    res$redirect(url)$send()
  })

  # I could not convince curl to redirect a POST to a GET, so both are good
  app$all("/login/redirect", function (req, res) {

    code <- req$query$code
    state <- req$query$state
    if (is.null(code) || is.null(state)) {
      res$
        set_status(400L)$
        send("Invalid request via auth server, no 'code' or 'state'.")
      return()
    }
    if (state != app$locals$state) {
      res$
        set_status(400L)$
        send("Unknown state in request via auth server")
      return()
    }

    # Get a token
    handle <- curl::new_handle()
    data <- charToRaw(paste0(
      "grant_type=authorization_code&",
      "code=", code, "&",
      "client_id=", app$locals$client_id, "&",
      "client_secret=", app$locals$client_secret, "&",
      "redirect_uri=", req$url
    ))
    curl::handle_setheaders(
      handle,
      "content-type" = "application/x-www-form-urlencoded"
    )
    curl::handle_setopt(
      handle,
      customrequest = "POST",
      postfieldsize = length(data),
      postfields = data
    )

    resp <- curl::curl_fetch_memory(app$locals$token_url, handle = handle)

    if (resp$status_code != 200L) {
      res$
        set_status(500L)$
        send(paste0(
          "Failed to acquire authorization token. ",
          rawToChar(resp$content)
        ))
      return()
    }

    res$send_json(text = rawToChar(resp$content))
  })
}
