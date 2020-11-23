load_all()
# create and launch app
oauth2_app <- oauth2_app(client_id = "123", client_secret = "pssst")
process <- local_app_process(oauth2_app)

# request code,
# need to add
url <- httr::modify_url(
  process$url(),
  path = "authorize",
  query = list(client_id = "123", client_secret = "pssst",
               state = "stateABC")
)

resp <- httr::GET(url)
httr::stop_for_status(resp)

httr::content(resp)

### Things that error

url <- httr::modify_url(
  process$url(),
  path = "authorize",
  query = list(client_id = "lala")
)

resp <- httr::GET(url)
httr::stop_for_status(resp)

url <- httr::modify_url(
  process$url(),
  path = "authorize",
  query = list(client_id = "lala", client_secret = "lili")
)

resp <- httr::GET(url)
httr::stop_for_status(resp)
