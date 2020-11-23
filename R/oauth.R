oauth2_app <- function(
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

        fs::dir_create("views")
        writeLines(
          "<body><p>Hey would you authorize the Third-Party App {app} to access your account?</p><p><a href='{url}/allow?state={state}'>Continue</a>
</p></body>
",
          file.path("views", "authorize.html")
          )
        txt <- res$render("authorize", locals = list(app = corresponding_app$app_name[1],
                                                     state = req$query$state,
                                                     url = sub("/authorize.*", "",req$url)))

        res$
          set_status(200L)$
          set_type("text/html")$
          send(txt)

      }
    }


  })

  app$get("/allow", function(req, res) {

    code <- sodium::bin2hex(sodium::random(15))

    req$app$locals$codes <- c(req$app$locals$codes, code)

    res$
      set_status(200L)$
      send_json(list(
        url = sub(req$url, "allow.*", glue::glue("cb?state={req$query$state}&code={code}"))
      ))
  })

  return(app)
}
