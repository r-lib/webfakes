
#' Fake OAuth 2.0 resource and authorization app
#'
#' @includeRmd man/rmd-fragments/oauth2.Rmd description
#'
#' @details
#' The app has the following endpoints:
#' * `GET /register` is the endpoint that you can use to register
#'   your third party app. It needs to receive the `name` of the
#'   third party app, and its `redirect_uri` as query parameters,
#'   otherwise returns an HTTP 400 error. On success it returns a
#'   JSON dictionary with entries `name` (the name of the third party
#'   app), `client_id`, `client_secret` and `redirect_uri`.
#' * `GET /authorize` is the endpoint where the user of the third
#'   party app is sent. You can change the URL of this endpoint with
#'   the `authorize_endpoint` argument. It needs to receive the `client_id`
#'   of the third party app, and its correct `redirect_uri` as query
#'   parameters. It may receive a `state` string as well, which can
#'   be used by a client to identify the request. Otherwise it
#'   generates a random `state` string. On error it fails with a HTTP
#'   400 error. On success it returns a simple HTML login page.
#' * `POST /authorize/decision` is the endpoint where the HTML login
#'   page generated at `/authorize` connects back to, either with a
#'   positive or negative result. The form on the login page will send
#'   the `state` string and the user's choice in the `action` variable.
#'   If the user authorized the third party app, then they are
#'   redirected to the `redirect_uri` of the app, with a temporary
#'   `code` and the `state` string supplied as query parameters.
#'   Otherwise a simple HTML page is returned.
#' * `POST /token` is the endpoint where the third party app requests
#'   a temporary access token. It is also uses for refreshing an
#'   access token with a refresh token. You can change the URL of this
#'   endpoint with the `token_endpoint` argument.
#'   To request a new token or refresh an existing one, the following
#'   data must be included in either a JSON or an URL encoded request body:
#'   - `grant_type`, this must be `authorization_code` for new tokens,
#'     and `refresh_token` for refreshing.
#'   - `code`, this must be the temporary code obtained from the
#'     `/authorize/decision` redirection, for new tokens. It is not
#'     needed when refreshing.
#'   - `client_id` must be the client id of the third party app.
#'   - `client_secret` must be the client secret of the third party
#'     app.
#'   - `redirect_uri` must be the correct redirection URI of the
#'     third party app. It is not needed when refreshing tokens.
#'   - `refresh_token` must be the refresh token obtained previously,
#'     when refreshing a token. It is not needed for new tokens.
#'   On success a JSON dictionary is returned with entries:
#'   `access_token`, `expiry` and `refresh_token`. (The latter is
#'   omitted if the `refresh` argument is `FALSE`).
#' * `GET /locals` returns a list of current apps, access tokens and
#'   refresh tokens.
#' * `GET /data` is an endpoint that returns a simple JSON response,
#'   and needs authorization.
#'
#' ## Notes
#'
#' * Using this app in your tests requires the glue package, so you
#'   need to put it in `Suggests`.
#' * You can add custom endpoints to the app, as needed.
#' * If you need authorization in your custom endpoint, call
#'   `app$is_authorized()` in your handler:
#'   ```
#'   if (!app$is_authorized(req, res)) return()
#'   ```
#'   `app$is_authorized()` returns an HTTP 401 response if the
#'   client is not authorized, so you can simply return from your
#'   handler.
#'
#' For more details see `vignette("oauth", package = "webfakes")`.
#'
#' @section `oauth2_resource_app()`:
#' App representing the API server (resource/authorization)
#' @return a `webfakes` app
#' @param access_duration After how many seconds should access tokens
#'   expire.
#' @param refresh_duration After how many seconds should refresh
#'   tokens expire (ignored if `refresh` is `FALSE`).
#' @param refresh Should a refresh token be returned (logical).
#' @param seed Random seed used when creating tokens. If `NULL`,
#'   we rely on R to provide a seed. The app uses its own RNG stream,
#'   so it does not affect reproducibility of the tests.
#' @param authorize_endpoint The authorization endpoint of the resource
#'   server. Change this from the default if the real app that you
#'   are faking does not use `/authorize`.
#' @param token_endpoint The endpoint to request tokens. Change this if the
#'   real app that you are faking does not use `/token`.
#' @return webfakes app
#'
#' @export
#' @family OAuth2.0 functions

oauth2_resource_app <- function(access_duration = 3600L,
                                refresh_duration = 7200L,
                                refresh = TRUE, seed = NULL,
                                authorize_endpoint = "/authorize",
                                token_endpoint = "/token") {

  access_duration
  refresh_duration
  refresh
  seed
  authorize_endpoint
  token_endpoint

  app <- new_app()
  app$locals$seed <- seed %||% get_seed()

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

  app$get(authorize_endpoint, function(req, res) {

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

  app$post(paste0(authorize_endpoint, "/decision"), function(req, res) {
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

  local_app_seed <- function(.local_envir = parent.frame()) {
    old_seed <- get_seed()
    set_seed(app$locals$seed)
    defer({
      app$locals$seed <- get_seed()
      set_seed(old_seed)
    }, envir = .local_envir)
  }

  new_access_token <- function(client_id, duration) {
    local_app_seed()
    token <- paste0("token-", generate_token())

    new <- data.frame(
      stringsAsFactors = FALSE,
      client_id = client_id,
      token = token,
      expiry = Sys.time() + duration
    )

    app$locals$tokens <- rbind(app$locals$tokens, new)
    token
  }

  new_refresh_token <- function(client_id, duration) {
    local_app_seed()
    token <- paste0("refresh-token-", generate_token())

    new <- data.frame(
      stringsAsFactors = FALSE,
      client_id = client_id,
      token = token,
      expiry = Sys.time() + duration
    )

    app$locals$refresh_tokens <- rbind(app$locals$refresh_tokens, new)
    token
  }

  expire_tokens <- function() {
    if (!is.null(app$locals$tokens)) {
      app$locals$tokens <- app$locals$tokens[
        app$locals$tokens$expiry > Sys.time(),,
        drop = FALSE
      ]
    }
    if (!is.null(app$locals$refresh_tokens)) {
      app$locals$refresh_tokens <- app$locals$refresh_tokens[
        app$locals$refresh_tokens$expiry > Sys.time(),,
        drop = FALSE
      ]
    }
  }

  is_valid_token <- function(token) {
    expire_tokens()
    token %in% app$locals$tokens$token
  }

  is_valid_refresh_token <- function(client_id, token) {
    expire_tokens()
    wh <- match(token, app$locals$refresh_tokens$token)
    if (is.na(wh)) return(FALSE)
    # client ID must match as well
    app$locals$refresh_tokens$client_id == client_id
  }

  # For refresh tokens
  app$post(token_endpoint, function(req, res) {
    if (req$form$grant_type != "refresh_token") return("next")
    client_id <- req$form$client_id
    tpapps <- app$locals$tpapps
    if (! client_id %in% tpapps$client_id) {
      res$
        set_status(400L)$
        send("Invalid client id")
      return()
    }
    tprec <- tpapps[match(client_id, tpapps$client_id), ]

    if (req$form$client_secret %||% "" != tprec$client_secret) {
      res$
        set_status(400L)$
        send("Invalid token request, client secret mismatch")
      return()
    }

    if (!is_valid_refresh_token(client_id, req$form$refresh_token)) {
      res$
        set_status(400L)$
        send_json(list(error = "invalid_request"), auto_unbox = TRUE)
      return()
    }

    res$send_json(list(
      access_token = new_access_token(client_id, access_duration),
      expiry = access_duration,
      refresh_token = new_refresh_token(client_id, refresh_duration)
    ), auto_unbox = TRUE)
  })

  # For regular tokens
  app$post(token_endpoint, function(req, res) {
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

    client_id <- req$form$client_id
    if (! client_id %in% tpapps$client_id) {
      res$
        set_status(400L)$
        send("Invalid client id")
      return()
    }

    tprec <- tpapps[match(client_id, tpapps$client_id), ]

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

    app$locals$codes <- setdiff(app$locals$codes, req$query$code)

    res$send_json(list(
      access_token = new_access_token(client_id, access_duration),
      expiry = access_duration,
      refresh_token =
        if (refresh) new_refresh_token(client_id, refresh_duration)
      ), auto_unbox = TRUE, pretty = TRUE)
  })

  app$get("/locals", function(req, res) {
    res$
      set_status(200L)$
      send_json(list(
        apps = app$locals$tpapps,
        access = app$locals$tokens,
        refresh = app$locals$refresh_tokens
      ), auto_unbox = TRUE)
  })

  app$is_authorized <- function(req, res) {
    expire_tokens()
    if (!("Authorization" %in% names(req$headers))) {
      res$
        set_status(401L)$
        send("Missing bearer token")
      return(FALSE)
    }

    token <- gsub("Bearer ", "", req$headers$Authorization[[1]])
    if (!is_valid_token(token)) {
      res$
        set_status(401L)$
        send("Invalid bearer token")
      return(FALSE)
    }
    TRUE
  }

  app$get("/data", function(req, res) {
    if (!app$is_authorized(req, res)) return()
    res$send_json(list(data = "top secret!"))
  })

  app
}

#' App representing the third-party app
#'
#' @includeRmd man/rmd-fragments/oauth2.Rmd description
#'
#' @details
#' Endpoints:
#' * `POST /login/config` Use this endpoint to configure the client ID
#'   and the client secret of the app, received from
#'   [oauth2_resource_app()] (or another resource app). You need to
#'   send in a JSON or URL encoded body:
#'   - `auth_url`, the authorization URL of the resource app.
#'   - `token_url`, the token URL of the resource app.
#'   - `client_id`, the client ID, received from the resource app.
#'   - `client_secret` the client secret, received from the resource
#'     app.
#' * `GET /login` Use this endpoint to start the login process. It
#'   will redirect to the resource app for authorization and after the
#'   OAuth2.0 dance to `/login/redirect`.
#' * `GET /login/redirect`, `POST /login/redirect` This is the
#'   redirect URI of the third party app. (Some HTTP clients redirect
#'   a `POST` to a `GET`, others don't, so it has both.) This endpoint
#'   is used by the resource app, and it received the `code` that can
#'   be exchanged to an access token and the `state` which was
#'   generated in `/login`. It contanct the resource app to get an
#'   access token, and the stores the token in its `app$locals`
#'   local variables. It fails with HTTP code 500 if it cannot obtain
#'   an access token. On success it returns a JSON dictionary with
#'   `access_token`, `expiry` and `refresh_token` (optionally) by
#'   default. This behavior can be changed by redefining the
#'   `app$redirect_hook()` function.
#' * `GET /locals` returns the tokens that were obtained from the
#'   resource app.
#' * `GET /data` is an endpoint that uses the obtained token(s) to
#'   connect to the `/data` endpoint of the resource app. The `/data`
#'   endpoint of the resource app needs authorization. It responds
#'   with the response of the resource app. It tries to refresh the
#'   access token of the app if needed.
#'
#' For more details see `vignette("oauth", package = "webfakes")`.
#'
#' @param name Name of the third-party app
#' @return webfakes app
#' @export
#' @family OAuth2.0 functions

oauth2_third_party_app <- function(name = "Third-Party app") {
  app <- new_app()
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

    tokens <- rawToChar(resp$content)
    app$locals$tokens <- jsonlite::fromJSON(tokens)

    app$redirect_hook(res, tokens)
  })

  app$redirect_hook <- function(res, tokens) {
    res$
      send_json(text = tokens)
  }

  app$get("/locals", function(req, res) {
    res$
      set_status(200L)$
      send_json(app$locals$tokens, auto_unbox = TRUE)
  })

  get_data <- function() {
    auth <- paste("Bearer", app$locals$tokens$access_token)
    handle <- curl::new_handle()
    curl::handle_setheaders(handle, Authorization = auth)
    url <- modify_path(app$locals$token_url, "/data")
    curl::curl_fetch_memory(url, handle = handle)
  }

  try_refresh <- function() {
    refresh_token <- app$locals$tokens$refresh_token
    if (is.null(refresh_token)) return(FALSE)

    data <- charToRaw(paste0(
      "refresh_token=", refresh_token, "&",
      "grant_type=refresh_token"
    ))
    handle <- curl::new_handle()
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

    if (resp$status_code != 200L) return(FALSE)

    tokens <- rawToChar(resp$content)
    app$locals$tokens <- jsonlite::fromJSON(tokens)

    TRUE
  }

  app$get("/data", function(req, res) {
    resp <- get_data()

    if (resp$status_code == 401) {
      if (try_refresh()) resp <- get_data()
    }

    res$
      set_status(resp$status_code)$
      set_type(resp$type)$
      send(resp$content)
  })
}

#' Helper function to log in to a third party OAuth2.0 app without a
#' browser
#'
#' It works with [oauth2_resource_app()], and any third party app,
#' including the fake [oauth2_third_party_app()].
#'
#' See `test-oauth.R` in webfakes for an example.
#'
#' @param login_url The login URL of the third party app.
#' @return A named list with
#' * `login_response` The curl HTTP response object for the login
#'   page.
#' * `token_response` The curl HTTP response object for submitting
#'   the login page.
#'
#' @family OAuth2.0 functions
#' @export

oauth2_login <- function(login_url) {
  login_resp <- curl::curl_fetch_memory(login_url)

  html <- rawToChar(login_resp$content)
  xml <- xml2::read_html(html)
  form <- xml2::xml_find_first(xml, "//form")
  input <- xml2::xml_find_first(form, "//input")

  actn <- xml2::xml_attr(form, "action")
  stnm <- xml2::xml_attr(input, "name")
  stvl <- xml2::xml_attr(input, "value")

  data <- charToRaw(paste0(
    stnm, "=", stvl, "&",
    "action=yes"
  ))

  handle2 <- curl::new_handle()
  curl::handle_setheaders(
    handle2,
    "content-type" = "application/x-www-form-urlencoded"
  )
  curl::handle_setopt(
    handle2,
    customrequest = "POST",
    postfieldsize = length(data),
    postfields = data
  )

  psurl <- parse_url(login_resp$url)
  actn_url <- paste0(psurl$protocol, "://", psurl$host, actn)
  token_resp <- curl::curl_fetch_memory(actn_url, handle = handle2)

  list(
    login_response = login_resp,
    token_response = token_resp
  )
}

#' Helper function to use httr's OAuth2.0 functions
#' non-interactively, e.g. in test cases
#'
#' To perform an automatic acknowledgement and log in for a
#' local OAuth2.0 app, run by httr, wrap the expression that
#' obtains the OAuth2.0 token in `oauth2_httr_login()`.
#'
#' In interactive sessions, `oauth2_httr_login()` overrides the
#' `browser` option, and when httr opens a browser page, it
#' calls [oauth2_login()] in a subprocess.
#'
#' In non-interactive sessions, httr does not open a browser page,
#' only messages the user to do it manually. `oauth2_httr_login()`
#' listens for these messages, and calls [oauth2_login()] in a
#' subprocess.
#'
#' @param expr Expression that calls [httr::oauth2.0_token()],
#'   either directly, or indirectly.
#' @return The return value of `expr`.
#'
#' @seealso See `?vignette("oauth", package = "webfakes")` for a case
#' study that uses this function.
#'
#' @export
#' @family OAuth2.0 functions

oauth2_httr_login <- function(expr) {
  proc <- NULL
  if (interactive()) {
    local_options(browser = function(url) {
      proc <<- callr::r_bg(
        oauth2_login,
        list(url),
        package = "webfakes"
      )
    })
    expr
  } else {
    withCallingHandlers(
      expr,
      message = function(msg) {
        if (grepl("^Please point your browser to the following url:",
                  msg$message)) {
          invokeRestart("muffleMessage")
        }
        if (grepl("^http", msg$message)) {
          proc <<- callr::r_bg(
            oauth2_login,
            list(trimws(msg$message)),
            package = "webfakes"
          )
          invokeRestart("muffleMessage")
        }
      }
    )
  }
}
