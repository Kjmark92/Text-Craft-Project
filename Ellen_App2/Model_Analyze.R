

run_lda <- function(clean_data,corpus_name,response_name,selected_ngram,selected_seed,selected_number_words,selected_number_topics){

  #selected_ngram = 1
  #selected_number_topics = 4
  #selected_seed = 1234
  #selected_number_words = 5
  
  #clean_data <- clean_model
  
  clean_model <- clean_data %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = selected_ngram) %>%
    ungroup()
  
  clean_model$word <- gsub('[[:punct:]]+', '', clean_model$word)
  clean_model$word <- gsub('[[:digit:]]+', '', clean_model$word)
  
  clean_model <- clean_model %>%
    mutate(word = lemmatize_words(word)) %>%
    filter(!word %in% stop_words$word, !word == "") %>%
    ungroup()
  
  # Karina Test
  if (selected_ngram == 2) {
    clean_model <- clean_model %>%
      separate(word, c("word1", "word2"), sep = " ", remove = F) %>%
      mutate(condition = if_else(word1 == word2, "TRUE", "FALSE")) %>%
      filter(condition == "FALSE") %>%
      ungroup()
  }
  

  wordcount_per_doc <- clean_model %>%
    select(id, word) %>%
    count(id,word)

  DocumentTermMatrix <- wordcount_per_doc %>%
    cast_dtm(id, word, n)

  lda_res <- LDA(DocumentTermMatrix, k = selected_number_topics, control = list(seed = selected_seed))
  topic_word_density <- tidy(lda_res, matrix = "beta")


  top_words_per_topic <- topic_word_density %>%
    group_by(topic) %>%
    top_n(selected_number_words, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  plot_topic_word_density <- top_words_per_topic %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()+ theme_minimal() + 
    labs(y = "Topic-Word Density (Beta)", x = "Top Words" , title = "Topic-Word Density for all topics")

  newlist <- list(plot_topic_word_density = plot_topic_word_density)

  return(newlist)

}









run_sentiment <- function(clean_data,corpus_name,response_name,selected_ngram,selected_seed,selected_number_words,selected_number_topics){
  
  
  bing <- get_sentiments("bing") 
  nrc <- get_sentiments("nrc")
  
  
  #Top sentiment comparison word cloud - PLOT 1
  
  t_t <- clean_data[[(corpus_name)]]
  
  s <- get_nrc_sentiment(t_t)
  sent_ <- colSums(s)
  sent_sum <- data.frame(count = sent_, sentiment = names(sent_))
  sent_sum$sentiment= factor(sent_sum$sentiment, levels = sent_sum$sentiment[order(sent_sum$count, decreasing = TRUE)])
  
  
  ang <- paste(t_t[s$anger>0], collapse = " ")
  ant <- paste(t_t[s$anticipation>0], collapse = " ")
  disg <- paste(t_t[s$disgust>0], collapse = " ")
  fear <- paste(t_t[s$fear>0], collapse = " ")
  joy <- paste(t_t[s$joy>0], collapse = " ")
  sad <- paste(t_t[s$sadness>0], collapse = " ")
  surp <- paste(t_t[s$surprise>0], collapse = " ")
  trust <- paste(t_t[s$trust>0], collapse = " ")
  
  wc_sents <- c(ang, ant, disg, fear, joy, sad, surp, trust)
  
  wc_corpus <- Corpus(VectorSource(wc_sents))
  
  wc_tdm <- TermDocumentMatrix(wc_corpus)
  wc_tdm <- as.matrix(wc_tdm)
  
  colnames(wc_tdm) <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  
  comparisioncloud_per_sentiment <- comparison.cloud(wc_tdm, random.order = TRUE, 
                   colors = brewer.pal(8,"Dark2"),
                   title.size = 0.6, max.words = 40,
                   scale = c(1, 1),
                   rot.per = 0.2)
  
  
  
  
  # General Sentiment count in whole data - PLOT 2
  
  
  
  nrc_sent_all <- clean_data %>%
    unnest_tokens(word, !!sym(corpus_name)) %>%
    count(word) %>%
    inner_join(nrc ,by=c("word")) %>%
    group_by(sentiment) %>%
    summarize(total=sum(n))
  
  sentiment_freq_total <- nrc_sent_all %>%
    ggplot(aes(x=reorder(sentiment,total),y=total,fill=sentiment)) + geom_bar(stat='identity',position='dodge')+ 
    ylab('Word Count') +
    xlab('Sentiment') +
    labs(title='Sentiment Frequency') + 
    theme(axis.text.x  = element_text(angle=45)) + theme_minimal()
  
  

  
  
  
  
  #Sentiment Frequency per response - PLOT 3
  
  
  sentiment.orientation <- data.frame(orientation = c(rep("Positive",5),rep("Negative",5)),
                                      sentiment = c("anticipation","joy","positive","trust","surprise","anger","disgust","fear","negative","sadness"))
  
  
  nrc_sent <- clean_data %>%
    unnest_tokens(word, !!sym(corpus_name)) %>%
    count(!!sym(response_name),word) %>%
    group_by(!!sym(response_name)) %>%
    inner_join(nrc ,by=c("word")) %>%
    group_by(!!sym(response_name),sentiment) %>%
    summarize(total=sum(n))
  
  
  response_groups <- levels(factor(clean_data[[response_name]]))
  
  sentiment_per_response_list <- list()
  for (i in seq_along(response_groups)){
    sentiment_per_response <- nrc_sent %>%
            filter(!!sym(response_name) == response_groups[i]) %>%
            ggplot(aes(x=reorder(sentiment,total),y=total, fill = sentiment)) + geom_bar(stat='identity',position='dodge')+ 
            ylab('Word Count') +
            xlab('Sentiment') +
            labs(title=paste('Sentiment Frequency for', response_groups[i])) + 
            theme(axis.text.x  = element_text(angle=45))+ theme_minimal()
    sentiment_per_response_list[[i]] <- sentiment_per_response
  }
  
  #sentiment_freq_per_response <- grid.arrange(
  #  grobs = sentiment_per_response_list,
  #  nrow = length(sentiment_per_response_list),
  #  ncol = 1,
  #  top = "Sentiment Frequency per Response",
  #)
  
  
  
  # Average Sentiment per Response - PLOT 4
  
  sentr_sent <- clean_data %>%
    get_sentences() %>%
    sentiment_by(by = as.character(response_name))
  
  average_sentiment_per_response <- ggplot(sentr_sent, aes(x = reorder(!!sym(response_name), ave_sentiment), y = ave_sentiment, color = !!sym(response_name))) + 
    geom_point(size=5,show.legend = F) + labs(x = 'Response', y = 'Average Sentiment', title = "Average Sentiment per Response")+ theme_minimal()
  
  
  
  # Mean SD Sentiment Score per response - PLOT 5
  
  #ggridges plot for sentiment distribution per response group
  clean_data$num <- seq.int(nrow(clean_data))
  
  sentr_byrev <- clean_data %>%
    get_sentences() %>%
    sentiment_by(by = 'num') %>%
    left_join(select(clean_data,num,!!sym(response_name)))
  
  #ggridges plot
  sentiment_stats_per_response <- sentr_byrev %>%
    mutate(r_levels=factor(!!sym(response_name))) %>% 
    ggplot(aes(x = ave_sentiment, y = r_levels, group = r_levels,fill=r_levels)) +
    geom_density_ridges(scale = 2.0, size = 0.25,alpha=0.4,show.legend=F) +
    scale_x_continuous(limits=c(-.2, 0.8), expand = c(0.01, 0)) +
    theme_bw() + 
    geom_vline(aes(xintercept=0)) + 
    labs(x = 'Sentiment', y = 'Response Group', title = 'Distribution of Sentiment by Response Group')+ theme_minimal()
  
  
  
  #positive/negative sentiment distribution total - PLOT 6
  
  netsent_posneg <- clean_data %>%
    select( !!sym(corpus_name)) %>%
    unnest_tokens(word,!!sym(corpus_name)) %>%
    count(word) %>%
    inner_join(bing,by=c("word")) %>%
    group_by(sentiment) %>%
    summarize(total=sum(n)) %>%
    spread(sentiment,total) %>%
    rename(negative_count = negative, positive_count = positive) %>%
    mutate(netsentiment=positive_count-negative_count, total= negative_count + positive_count, negative = negative_count/total, positive = positive_count/total)
  
  
  netsent <- netsent_posneg %>%
    gather(key = sentiment_type, value = percentage, c(positive,negative))
  
  
  pos_neg_sentiment_total <- ggplot(netsent, aes(x = sentiment_type, y = percentage, fill = sentiment_type)) + 
    geom_bar(stat = 'identity', position = 'dodge') + labs(y = 'Percentage',x = 'Sentiment Type', title = 'Sentiment Distribution Overall')+ theme_minimal()
  
  
 
  
  #positive/negative sentiment distribution per response - PLOT 7 
  
  netsent_posneg <- clean_data %>%
    select(!!sym(response_name), !!sym(corpus_name)) %>%
    unnest_tokens(word,!!sym(corpus_name)) %>%
    count(!!sym(response_name),word) %>%
    group_by(!!sym(response_name)) %>%
    inner_join(bing,by=c("word")) %>%
    group_by(!!sym(response_name),sentiment) %>%
    summarize(total=sum(n)) %>%
    spread(sentiment,total) %>%
    rename(negative_count = negative, positive_count = positive) %>%
    mutate(netsentiment=positive_count-negative_count, total= negative_count + positive_count, negative = negative_count/total, positive = positive_count/total)
  

  netsent <- netsent_posneg %>%
    gather(key = sentiment_type, value = percentage, c(positive,negative))


  pos_neg_sentiment_per_response <- ggplot(netsent, aes(x = !!sym(response_name), y = percentage, fill = sentiment_type)) + 
    geom_bar(stat = 'identity', position = 'dodge') + labs(y = 'Sentiment Percentage',x= 'Response', title = 'Sentiment Distribution per Response')+ theme_minimal()
  

  
  
  
  
  newlist <- list(comparisioncloud_per_sentiment = comparisioncloud_per_sentiment,
                  sentiment_freq_total = sentiment_freq_total,
                  sentiment_per_response_list = sentiment_per_response_list,
                  average_sentiment_per_response = average_sentiment_per_response,
                  sentiment_stats_per_response = sentiment_stats_per_response,
                  pos_neg_sentiment_total = pos_neg_sentiment_total,
                  pos_neg_sentiment_per_response = pos_neg_sentiment_per_response
                  )
  
  return(newlist)
  
  
  
  
  
  
}










