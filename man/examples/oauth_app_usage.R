load_all()
# create and launch app
oauth2_server_app <- oauth2_server_app(client_id = "123", client_secret = "pssst")
oauth2_server_process <- local_app_process(oauth2_server_app)

oauth2_third_party_app <- oauth2_third_party_app(client_id = "123", client_secret = "pssst")
oauth2_third_party_process <- local_app_process(oauth2_third_party_app)

# request code,
# need to add
url <- "http://127.0.0.1:3000"
url <- oauth2_third_party_process$url()
url <- httr::modify_url(
  url,
  path = "login",
  query = list(server_url = oauth2_server_process$url(),
               self_url = url)
)

resp <- httr::GET(url)
resp

httr::GET(httr::content(resp))

file <- tempfile()
xml2::write_html(
  text,
  file)
browseURL(file)

### Things that error #######################

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
