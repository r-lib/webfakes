
#' OAuth 2.0
#'
#' @includeRmd man/rmd-fragments/oauth2.Rmd description
#'
#' @details
#' For more details see `vignette("oauth", package = "webfakes")`.
#'
#' @name oauth2.0
#' @aliases NULL

#' @section `oauth2_resource_app()`:
#' App representing the API server (resource/authorization)
#' @export
#' @return a `webfakes` app
#' @rdname oauth2.0
#' @param access_duration After how many seconds should the access token expire.
#' @param refresh_duration After how many seconds should the refresh token expire
#' (ignored if `refresh` is `FALSE`).
#' @param refresh Should a refresh token be returned (logical).
#' @param seed Random seed set when creating the app.
oauth2_resource_app <- function(access_duration = 3600L, refresh_duration = 7200L,
                                refresh = TRUE, seed = 42) {

  app <- new_app()

  app$locals$access_produced <- 0
  app$locals$refresh_produced <- 0

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

  app$set_config("views", system.file("views", package = "webfakes"))
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

  produce_access_token <- function(access_duration) {

    app$locals$access_produced <- app$locals$access_produced + 1
    set.seed(app$locals$access_produced + seed)

    token <- paste0("token-", generate_token())
    app$locals$tokens <- rbind(
      app$locals$tokens,
      data.frame(token = token, expiry = Sys.time() + access_duration)
    )
    return(token)
  }

  produce_refresh_token <- function(refresh_duration, refresh) {

    if (!refresh) {
      return(NA)
    }

    app$locals$refresh_produced <- app$locals$refresh_produced + 1
    set.seed(app$locals$refresh_produced + seed)

    refresh_token <- paste0("refresh_token-", generate_token())
    app$locals$refresh_tokens <- rbind(
      app$locals$refresh_tokens,
      data.frame(token = refresh_token, expiry = Sys.time() + refresh_duration)
    )
    return(refresh_token)
  }
  app$post("/token", function(req, res) {


    if (req$form$grant_type == "refresh_token") {

      app$locals$refresh_tokens <- app$locals$refresh_tokens[
        app$locals$refresh_tokens$expiry > Sys.time(),
        ]

      if (! (req$form$refresh_token %in% app$locals$refresh_tokens$token)) {
           res$
          set_status(400L)$
          send_json(list(error = "invalid_request"), auto_unbox = TRUE)
        return()
      }

      access_token <- produce_access_token(access_duration)
      refresh_token <- produce_refresh_token(refresh_duration, refresh)

      info <- list(access_token = access_token,
                   expiry = access_duration,
                   refresh_token = refresh_token)

      info <- info[!is.null(info)]

      res$
        send_json(
          info,
          auto_unbox = TRUE
        )
      return()
    }

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

    access_token <- produce_access_token(access_duration)
    refresh_token <- produce_refresh_token(
      refresh_duration = refresh_duration,
      refresh = refresh
      )

    app$locals$codes <- setdiff(app$locals$codes, req$query$code)

    res$
      send_json(
        list(access_token = access_token,
             expiry = access_duration,
             refresh_token = refresh_token),
        auto_unbox = TRUE
      )
  })

  app$get("/noninteractive", function(req, res) {
    access_token <- produce_access_token(access_duration)
    refresh_token <- produce_refresh_token(
      refresh_duration = refresh_duration,
      refresh = refresh
    )
    res$
      send_status(200L)
  })

  app$get("/locals", function(req, res) {

    res$
      set_status(200L)$
      send_json(list(tokens = app$locals$tokens, refresh = app$locals$refresh_tokens), auto_unbox = TRUE)
  })

  app$get("/data", function(req, res){
    app$locals$tokens <- app$locals$tokens[app$locals$tokens$expiry > Sys.time(),]
    if (!("Authorization" %in% names(req$headers))) {
      res$
        set_status(401L)$
        send("Missing bearer token")
    } else {
      token <- gsub("Bearer ", "", req$headers$Authorization[[1]])
      if (!token %in% app$locals$tokens$token) {
        res$
          set_status(401L)$
          send("Invalid bearer token")
      } else {
        res$
          send_json(list(data = "top secret!"))
      }
    }
  })

  app
}

#' @section `oauth2_third_party_app()`:
#' App representing the third-party app
#' @export
#' @rdname oauth2.0
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
