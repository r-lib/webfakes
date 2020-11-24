oauth2_server_app <- function(
  app_name = "Third-Party App",
  client_secret = "client_secret",
  client_id = "client_id"
  ) {
  # Create app
  app <- new_app()

  # Needed later
  app$engine("html", tmpl_glue())

  # Register third-party app
  app$locals$apps <- data.frame(
    app_name = app_name,
    client_secret = client_secret,
    client_id = client_id
  )

  # First step, asking the user for access
  app$get("/authorize", function(req, res) {

    # Missing information either client secret or client ID
    if (is.null(req$query$client_secret) || is.null(req$query$client_id)) {
      res$
        # How would I set a status message, maybe in the body
        send_status(400L)
    } else {

      corresponding_app <- res$app$locals$apps[
        res$app$locals$apps$client_secret == req$query$client_secret &&
          res$app$local$apps$client_id == req$query$client_id,]

      # Wrong client ID or token
      if (nrow(corresponding_app) == 0) {
        res$
          # How would I set a status message
          send_status(400L)
      } else {

        code <- sodium::bin2hex(sodium::random(15))

        req$app$locals$codes <- c(req$app$locals$codes, code)

        app <- corresponding_app$app_name[1]
        state <- req$query$state
        url <- req$query$redirect_uri

        txt <- glue::glue("<body><p>Hey would you authorize the Third-Party App {app} to access your account?</p><p><a href='{url}/cb?state={state}&code={code}'>Continue</a>
</p></body>")

        res$
          set_status(200L)$
          set_type("text/html")$
          send(txt)

      }
    }

    app$get("/token", function(req, res) {

      if (req$query$code) {

        token <- sodium::bin2hex(sodium::random(5))

        req$app$locals$tokens <- c(req$app$locals$tokens, token)
        req$app$locals$codes <- req$app$locals$codes[req$app$locals$codes != req$query$code]

        res$
          send_json(list(
            token = token
          ))

      } else {
        res$
          send_status(401L)
      }

    })

  })

  return(app)
}

oauth2_third_party_app <- function(
  client_secret = "client_secret",
  client_id = "client_id",
  server_url
) {
  app <- new_app()
  app$locals$client_secret <- client_secret
  app$locals$client_id <- client_id
  app$locals$server_url <- server_url

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
          redirect_uri = req$query$self_url
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
      send_json(content)

  })
}
