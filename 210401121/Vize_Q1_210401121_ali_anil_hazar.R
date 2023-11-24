library(httr)



spotify_token <- function() {
  # Set client ID and client secret
  client_id <- Sys.getenv("SPOTIFY_ID")
  client_secret <- Sys.getenv("SPOTIFY_SECRET")
  
  token_url <- 'https://accounts.spotify.com/api/token'
  auth <- paste(client_id, client_secret, sep = ":")
  auth <- base64enc::base64encode(charToRaw(auth))
  
  
  response <- httr::POST(
    url = token_url,
    httr::add_headers(
      Authorization = paste('Basic', auth)
    ),
    body = list(
      grant_type = 'client_credentials'
    ),
    encode = 'form'
  )
  
  content <- httr::content(response, as = "parsed")
  access_token <- content$access_token
  
  
  status_code <- httr::status_code(response)
  
  
  token <- httr::content(response)$access_token
  
  
  output <- list(
    status_code = status_code,
    token = paste('Bearer', token)
  )
  
  return(output)
}
spotify_token()

