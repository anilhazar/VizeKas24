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


token_result <- spotify_token() 
print(token_result)

search_results <- spotify_search_artist("The Doors")
print(search_results)

my_artist_list <- c("The Door", "The Weekend", "Sezen Aksu", "Barış Manço", "İbrahim Tatlıses") 
my_artists <- data.frame(artist = my_artist_list, id = character(length(my_artist_list)), stringsAsFactors = FALSE)

for (i in 1:nrow(my_artists)) {
  artist_name <- my_artists[i, "artist"]
  search_result <- spotify_search_artist(artist_name)
  id <- search_result$search_results$id[1] 
  my_artists[i, "id"] <- id
}

print(my_artists)
