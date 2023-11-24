
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

spotify_search_artist <- function(artist_name) {
  
  access_token <- spotify_token()$token
  access_token <- gsub("^Bearer\\s+", "", access_token)
  
 
  endpoint <- "https://api.spotify.com/v1/search"
  

  params <- list(
    q = artist_name,
    type = "artist"
  )
  
  
  response <- httr::GET(
    url = endpoint,
    httr::add_headers(
      Authorization = paste("Bearer ", access_token)
    ),
    query = params
  )
  
  
  status_code <- as.numeric(httr::status_code(response))
  
  if (status_code == 200) {
    
    content <- httr::content(response, as = "parsed")
    
    
    valid_results <- sapply(content$artists$items, function(item) !is.null(item$name) & item$name != "")
    
  
    artist_names <- sapply(content$artists$items[valid_results], function(item) item$name)
    artist_ids <- sapply(content$artists$items[valid_results], function(item) item$id)
    
   
    output <- list(
      status_code = status_code,
      search_results = data.frame(artist = artist_names, id = artist_ids, stringsAsFactors = FALSE)
    )
  } else {
    
    output <- list(
      status_code = status_code,
      search_results = NULL
    )
  }
  
 
  return(output)
}


result <- spotify_search_artist("The Weeknd")

print(result$search_results)