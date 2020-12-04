rsapp <- local_app_process(
  oauth2_resource_app(access_duration = 10L),
  opts = server_opts(num_threads = 3)
)

rsapp

regi_url <- rsapp$url("/register")
auth_url <- rsapp$url("/authorize")
toke_url <- rsapp$url("/token")
url <- paste0(
  regi_url,
  "?name=3P%20app2",
  "&redirect_uri=", httr::oauth_callback()
)
reg_resp <- httr::GET(url)
reg_resp
regdata <- httr::content(reg_resp)
regdata
app <- httr::oauth_app(
  "3P app2",
  key = regdata$client_id[[1]],
  secret = regdata$client_secret[[1]],
  redirect_uri = httr::oauth_callback()
)

endpoint <- httr::oauth_endpoint(
  authorize = auth_url,
  access = toke_url
)

token <- httr::oauth2.0_token(
  endpoint, app
)
httr::GET(rsapp$url("/data"))
httr::content(httr::GET(rsapp$url("/data"), config = token))
Sys.sleep(10)
httr::GET(rsapp$url("/data"), config = token)
