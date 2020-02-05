library(httr)
library(jsonlite)
library(tidyverse)

#Setup Yelp API
client_id <- "y0uHSQoir3gtmewwyYhLeA"
client_secret <- "IrNmBfj70C4h6bFYX7R5SWn_aHGNq01roJCA1RfX6Dy01k2W_FrXpx_VLxGHgfdS8YJhAYMYFavoEI8sQLnzy7e0lMovmn6ibFxMzu-GamnZiYIN5QTZ9NNa7BQiXnYx"

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))
token <- content(res)$access_token

#Search the ids related to cirque shows in LV and create it in id_names dataframes
yelp <- "https://api.yelp.com"
term <- "cirque du soleil"
location <- "LAS VEGAS, NV"
categories <- NULL
limit <- 50
radius <- 8800
url_id <- modify_url(yelp, path = c("v3", "businesses", "search"),
                     query = list(term = term, 
                                  location = location,
                                  limit = limit,
                                  radius = radius))
res <- GET(url_id, add_headers('Authorization' = paste("bearer", client_secret)))
results <- content(res)

yelp_httr_parse <- function(x) {
  parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     distance = x$distance)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data.frame(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state, 
                   distance= parse_list$distance)
}

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
id_names <- do.call("rbind", results_list)

id_names <- id_names %>%
  filter(str_detect(name, "Cirque")) %>%
  select(id, name, rating, review_count)

#Per Cirque's ids request the reviews
id_list <- id_names[1]
yelp2 <- "https://api.yelp.com/v3/businesses/"

review_httr_parse <- function(x) {
  parse_list <- list(id = x$id, 
                     rating = x$rating, 
                     text = x$text, 
                     time_created = x$time_created)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data.frame(id=parse_list$id,
                   rating = parse_list$rating, 
                   text = parse_list$text, 
                   time_created = parse_list$time_created)
}

url_rev <- function(id){
  url = paste0(yelp2, id, "/reviews")
  res <- GET(url, add_headers('Authorization' = paste("bearer", client_secret)))
  results <- content(res)
  results_list <- lapply(results$reviews, FUN = review_httr_parse)
  id_reviews <- do.call("rbind", results_list)
}

#Test before looping with functions
url_rev(id_names[1,1])

url = paste0(yelp2, id_names[1,1], "/reviews")
res <- GET(url, add_headers('Authorization' = paste("bearer", client_secret)))
results <- content(res)
results_list <- lapply(results$reviews, FUN = review_httr_parse)
id_reviews <- do.call("rbind", results_list)

#When everything works, loop the ids
#for (x in id_list){
#  url_rev(x)
#}

