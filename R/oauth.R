oauth2_app <- function(
  app_name = "Third-Party App",
  client_secret = "client_secret",
  client_id = "client_id"
  ) {
  # Create app
  app <- new_app()

  # Register third-party app
  app$locals$apps <- data.frame(
    app_name = app_name,
    client_secret = client_secret,
    client_id = client_id
  )

  app$use(mw_static(system.file("examples", "static", "public", package = "presser")))

  # How to get a token
  app$get("/authorize", function(req, res) {

    # Missing information either client secret or client ID
    if (is.null(req$query$client_secret) || is.null(req$query$client_id)) {
      res$
        # How would I set a status message
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

        code <- sodium::bin2hex(sodium::random(7))

        res$app$locals$authorization_codes <- c(res$app$locals$authorization_codes, code)

        res$
          set_status(200L)$
          send_json(
            list(
              url = sub(
                req$url, "/authorize.*",
                paste0("/cb?code=", code, "&state=", req$query$state)
                )
              )
            )
      }
    }


  })

  return(app)
}
