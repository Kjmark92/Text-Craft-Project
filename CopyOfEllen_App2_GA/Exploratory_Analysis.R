


#Sent to reactive - Cleaned Text Corpus
get_clean_data <- function(df,corpus_name,response_name,selected_remove_custom_words ){
  
  #c <- read_csv("Data/cirque_du_soleil_clean.csv")
  #df <- c
  #response_name <- "Show"
  #corpus_name <- "Text"
  
  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', '', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', '', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word == "") %>%
    ungroup()
  
  #selected_remove_custom_words <- c()
  clean_model <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words))
  
  
  #bring the individual words back into a text
  clean_model <- clean_model %>% 
    select(id, word, !!sym(response_name)) %>%
    group_by(id) %>%
    mutate(ind = row_number()) %>%
    spread(key = ind, value = word)
  
  clean_model [is.na(clean_model)] <- ""
  clean_model <- clean_model %>% unite( !!sym(corpus_name),-id, -!!sym(response_name), sep =" ")
  return(clean_model)
}




#Displayed output


# Overall Top Word Counts
get_overall_wordcount <- function(df,corpus_name,response_name,selected_remove_custom_words ){

  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', '', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', '', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word == "",!word %in% "NA") %>%
    ungroup()
  
  wordCount <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) %>%
    count(word,sort = TRUE) %>%
    rename(Word = word, Count = n) %>%
    slice(1:10)
  
  return(wordCount)
  
}



# Overall top tf-idf
get_overall_tfidf <- function(df,corpus_name,response_name,selected_remove_custom_words ){
  
  
  
  
  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', '', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', '', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word == "") %>%
    ungroup()
  
  
  clean_model <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) %>%
    count(id,word,sort = TRUE) 
  
  #get tf-idf for all words in all docs
  tfidf <- clean_model %>% bind_tf_idf(word,id,n) %>%
    mutate(word = fct_reorder(word,tf_idf)) %>%
    arrange(desc(tf_idf))
  
  # top 10 
  tfidf_top <- tfidf %>% select(word,tf_idf) %>% 
    rename(Word = word,`TF-IDF` = tf_idf) %>% slice(1:10)
  
  return(tfidf_top)
}



# Word Suggestions to remove
get_overall_suggestion <- function(df,corpus_name,response_name,selected_remove_custom_words ){

  
  clean_model <- df %>%
    mutate(id = row_number()) %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = 1) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', '', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', '', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word == "") %>%
    ungroup()
  
  
  clean_model <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) 
  
  # Get word counts
  word_counts_all <- clean_model %>%
    count(word,sort = TRUE) %>%
    rowwise() %>%
    mutate(Proportion = n/length(clean_model$word)) %>%
    rename(Word = word, Count = n)
  
  # Get tfidf
  tfidf_all <- clean_model %>%
    filter(!word %in% c(selected_remove_custom_words)) %>%
    count(id,word,sort = TRUE) %>% bind_tf_idf(word,id,n) %>%
    group_by(word) %>%
    summarise(Mean_tf_idf = mean(tf_idf)) %>%
    mutate(word = fct_reorder(word,Mean_tf_idf)) %>%
    select(word,Mean_tf_idf) %>%
    arrange(Mean_tf_idf) %>%
    rename(`TF-IDF` = Mean_tf_idf, Word = word)
  
  #combined
  combined <- tfidf_all %>% left_join(word_counts_all , by = "Word") 
  
  suggestions <- combined %>% filter(Proportion > 0.01 | `TF-IDF` < 0.03) %>%
    select(Word,Count,`TF-IDF`)
  

  return(suggestions)
}


