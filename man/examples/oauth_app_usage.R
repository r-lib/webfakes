load_all()
# create and launch apps

# the server
# we register the third-party app directly
oauth2_server_app <- oauth2_server_app(client_id = "123", client_secret = "pssst")
oauth2_server_process <- local_app_process(oauth2_server_app)

# the third-party app
oauth2_third_party_app <- oauth2_third_party_app(
  client_id = "123",
  client_secret = "pssst",
  server_url = oauth2_server_process$url())
oauth2_third_party_process <- local_app_process(oauth2_third_party_app)

# request code,
# need to add
url <- oauth2_third_party_process$url()
url <- httr::modify_url(
  url,
  path = "login"
)

resp <- httr::GET(url)
httr::content(resp)

#################################
load_all()
# create and launch apps

# the server
# we register the third-party app directly
oauth2_server_app <- oauth2_server_app(client_id = "123", client_secret = "pssst")
oauth2_server_process <- local_app_process(oauth2_server_app)

endpoint <- httr::oauth_endpoint(
  base_url = oauth2_server_process$url(),
  authorize = "authorize",
  access = "token"
)

app <- httr::oauth_app(
  appname = "Third-Party App",
  secret = "pssst",
  key = "123"
)

httr::oauth2.0_token(
  endpoint,
  app,
  query_authorize_extra = list(client_secret = "pssst")
  )
