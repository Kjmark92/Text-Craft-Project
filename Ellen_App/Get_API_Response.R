



test_api <- function(apilink = ""){

  if (apilink != "")  {
    baseurl <- apilink
    initialQuery <- fromJSON(baseurl)
    maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 
    pages <- list()
    #6 pages
    for(i in 0:20){
      nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
      message("Retrieving page ", i)
      pages[[i+1]] <- nytSearch 
      Sys.sleep(1) 
    }
    df <- rbind_pages(pages)
}

  else {
  df <- "no"
}  
  return(df)
}
