


#Sent to reactive
get_clean_data <- function(df,corpus_name,response_name,selected_remove_custom_words ){
  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', '', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', '', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word) %>%
    ungroup()
  
  clean_model <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words))
  
  
  #bring the individual words back into a text
  clean_model <- clean_model %>% 
    select(id, word, !!sym(response_name)) %>%
    group_by(id) %>%
    mutate(ind = row_number()) %>%
    spread(key = ind, value = word)
  
  clean_model [is.na(clean_model)] <- ""
  clean_model <- clean_model %>% unite( !!sym(corpus_name),-id, -!!sym(response_name), sep =" " )
  
  
  return(clean_model)
}




#Displayed output
get_overall_wordcount <- function(df,corpus_name,response_name,selected_remove_custom_words ){

  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', 'NA', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', 'NA', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word %in% "NA") %>%
    ungroup()
  
  wordCount <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) %>%
    count(word,sort = TRUE) %>%
    slice(1:10)
  
  return(wordCount)
  
}




get_overall_tfidf <- function(df,corpus_name,response_name,selected_remove_custom_words ){
  
  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', 'NA', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', 'NA', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word %in% "NA") %>%
    ungroup()
  
  wordCount <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) %>%
    count(word,sort = TRUE) %>%
    slice(1:10)
  
  return(wordCount)
}




get_overall_suggestion <- function(df,corpus_name,response_name,selected_remove_custom_words ){
  
  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', 'NA', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', 'NA', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word %in% "NA") %>%
    ungroup()
  
  wordCount <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) %>%
    count(word,sort = TRUE) %>%
    slice(1:10)
  
  return(wordCount)
}


