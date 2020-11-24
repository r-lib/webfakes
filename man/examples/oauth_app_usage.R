load_all()
# create and launch app
oauth2_server_app <- oauth2_server_app(client_id = "123", client_secret = "pssst")
oauth2_server_process <- local_app_process(oauth2_server_app)

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
  path = "login",
  query = list(server_url = oauth2_server_process$url(),
               self_url = url)
)

resp <- httr::GET(url)
resp

file <- tempfile()
xml2::write_html(
  httr::content(resp),
  file)
browseURL(file)

httr::GET("http://127.0.0.1:42539//cb?state=b7b980658c&code=31e4b4984b62fff95dbbe0a8ba0169")
