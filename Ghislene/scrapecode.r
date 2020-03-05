library(rvest)
library(tidyverse)
library(stringr)


## scraper function
ScrapeThePage <- function(theURL){
  
  reviews <- theURL %>% 
    html_nodes(".userPost") 
  
  nReviews <- length(reviews)
  
  comments <- reviews %>%
    html_nodes(".comment") %>%
    html_text() %>%
    as.data.frame(stringsAsFactors=F) %>%
    slice(seq(2,2*nReviews,2)) %>%
    rename(comment = ".") %>%
    mutate(comment = str_replace(comment,"Hide Full Comment",""),
           comment = str_replace(comment,"Comment:",""))
  
  regexp <- "[[:digit:]]+"
  
  stars <- reviews %>%
    html_nodes(".current-rating") %>%
    html_text() %>%
    as.data.frame(stringsAsFactors = F) %>%
    rename(value=".") %>%
    mutate(value = as.numeric(str_extract(value, regexp)))
  
  ratingCategory <- reviews %>%
    html_nodes(".category") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(variable=".") 
  
  ratingDF <- data.frame(stars = stars$value, 
                         variable = ratingCategory$variable) %>%
    mutate(rowID = rep(1:nReviews,each=3)) %>%
    spread(variable,stars)
  
  reviewerInfo <- reviews %>%
    html_nodes(".reviewerInfo") %>%
    html_text() %>%
    as.data.frame(stringsAsFactors=F) %>%
    rename(info = ".") %>%
    mutate(info = str_replace(info,"Reviewer:",""))
  
  
  conditionInfo <- reviews %>%
    html_nodes(".conditionInfo") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(condition=".") %>%
    mutate(condition = substring(condition, 19))
  
  helpful <- reviews %>%
    html_nodes(".helpful") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(helpful=".") %>%
    mutate(helpful = as.numeric(str_extract(helpful, regexp)))
  
  date <- reviews %>%
    html_nodes(".date") %>%
    html_text() 
  
  
  ## collect everything
  
  theReviews <- data.frame(
    reviewer = reviewerInfo$info,
    select(ratingDF,-rowID),
    conditionInfo,
    comments,
    date,
    helpful,
    stringsAsFactors = F
  )
  
}

##
## Put the whole thing into a function 
##

## @knitr ScrapeWebMDFunction
ScrapeDrugWebMD <- function(baseURL){
  
  GetNumberReviews <- read_html(paste0(baseURL,'&pageIndex=0&sortby=3&conditionFilter=-1')) %>%
    html_nodes(".postPaging") %>%
    html_text() 
  
  NumberReviews <- as.numeric(str_split(GetNumberReviews[1]," ")[[1]][4])
  ReviewPerPage <- 5
  NumberPages <- floor(NumberReviews/ReviewPerPage)  
  eps = NumberPages - (NumberReviews/ReviewPerPage)
  if (eps==0){NumberPages <- NumberPages-1}
  
  
  ## run the scraper
  allReviews <- NULL
  
  for (thePageIndex in 0:NumberPages){
    
    #for (thePageIndex in 0:10){
    pageURL <- read_html(paste0(baseURL,'&pageIndex=',thePageIndex,'&sortby=3&conditionFilter=-1'))
    theReviews <- ScrapeThePage(pageURL)
    
    allReviews <- bind_rows(allReviews,
                            theReviews)
  }
  
  return(allReviews)
  
}



## Celexa
theURL <- 'https://www.webmd.com/drugs/drugreview-8603-Celexa-oral.aspx?drugid=8603&drugname=Celexa-oral'
theReviews <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews,'Ghislene/data/Celexa.rds')

## Prozac
theURL <-'https://www.webmd.com/drugs/drugreview-6997-Prozac-oral.aspx?drugid=6997&drugname=Prozac-oral'
theReviews_prozac <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_prozac,'Ghislene/data/Prozac.rds')

## Lexapro
theURL <- 'https://www.webmd.com/drugs/drugreview-63990-Lexapro+oral.aspx?drugid=63990&drugname=Lexapro+oral'
theReviews_lexapro <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_lexapro,'Ghislene/data/Lexapro.rds')

## Zoloft
theURL <- 'https://www.webmd.com/drugs/drugreview-35-Zoloft+oral.aspx?drugid=35&drugname=Zoloft+oral'
theReviews_zoloft <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_zoloft,'Ghislene/data/Zoloft.rds')

## Luvox
theURL <- 'https://www.webmd.com/drugs/drugreview-1089-Luvox-oral.aspx?drugid=1089&drugname=Luvox-oral'
theReviews_luvox <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_luvox,'Ghislene/data/Luvox.rds')

## Paxil
theURL <- 'https://www.webmd.com/drugs/drugreview-6968-Paxil-oral.aspx?drugid=6968&drugname=Paxil-oral'
theReviews_paxil <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_paxil,'Ghislene/data/Paxil.rds')


## Zoloft
theURL <- 'https://www.webmd.com/drugs/drugreview-35-Zoloft+oral.aspx?drugid=35&drugname=Zoloft+oral'
theReviews_zoloft <- ScrapeDrugWebMD(theURL)
theReviews_zoloft <- theReviews_zoloft%>%
  mutate(drug_name = "Zoloft")
saveRDS(theReviews_zoloft,'Ghislene/data/Zoloft.rds')

## Hydrocodone (3000 reviews, painreliever narcotic, cough suppressant)
theURL <- 'https://www.webmd.com/drugs/drugreview-251-hydrocodone-acetaminophen-oral.aspx?drugid=251&drugname=hydrocodone-acetaminophen-oral'
theReviews_hydrocodone <- ScrapeDrugWebMD(theURL)
theReviews_hydrocodone <- theReviews_hydrocodone%>%
  mutate(drug_name = "Hydrocodone")
saveRDS(theReviews_hydrocodone,'Ghislene/data/Hydrocodone.rds')

## Neurontin (2000 reviews, gabapentin, can treat pain and seizures)
theURL <- 'https://www.webmd.com/drugs/drugreview-9845-Neurontin-oral.aspx?drugid=9845&drugname=Neurontin-oral'
theReviews_Neurontin <- ScrapeDrugWebMD(theURL)
theReviews_Neurontin <- theReviews_Neurontin%>%
  mutate(drug_name = "Neurontin")
saveRDS(theReviews_Neurontin,'Ghislene/data/Neurontin.rds')

## Vicodin (1421, opiates)
theURL <- 'https://www.webmd.com/drugs/drugreview-3459-Vicodin-oral.aspx?drugid=3459&drugname=Vicodin-oral'
theReviews_Vicodin <- ScrapeDrugWebMD(theURL)
theReviews_Vicodin <- theReviews_Vicodin%>%
  mutate(drug_name = "Vicodin")
saveRDS(theReviews_Vicodin,'Ghislene/data/Vicodin.rds')

## Xanax (2950 reviews,antianxiety)
theURL <- 'https://www.webmd.com/drugs/drugreview-9824-xanax.aspx?drugid=9824&drugname=xanax'
theReviews_Xanax<- ScrapeDrugWebMD(theURL)
theReviews_Xanax <- theReviews_Xanax%>%
  mutate(drug_name = "Xanax")
saveRDS(theReviews_Xanax,'Ghislene/data/Xanax.rds')

## Percocet(1541 reviews, opiates)
theURL <- 'https://www.webmd.com/drugs/drugreview-7277-percocet.aspx?drugid=7277&drugname=percocet'
theReviews_Percocet<- ScrapeDrugWebMD(theURL)
theReviews_Percocet<- theReviews_Percocet%>%
  mutate(drug_name = "Percocet")
saveRDS(theReviews_Percocet,'Ghislene/data/Percocet.rds')

meddata2<-rbind(theReviews_zoloft,theReviews_hydrocodone,theReviews_Neurontin,theReviews_Vicodin,theReviews_Xanax,theReviews_Percocet)
write.csv(meddata2,'Ghislene/data/meddata2.csv', row.names = TRUE)
saveRDS(meddata2,'Ghislene/data/meddata2.rds')
