

run_lda <- function(clean_data,corpus_name,response_name,selected_ngram,selected_seed,selected_burning,selected_number_topics){

  clean_model <- clean_data %>%
    group_by(!!sym(response_name)) %>%
    unnest_tokens(word, !!sym(corpus_name), token = "ngrams", n = selected_ngram) %>%
    ungroup()
  
  
  wordcount_per_doc <- clean_model %>%
    select(id, word) %>%
    count(id,word)

  DocumentTermMatrix <- wordcount_per_doc %>%
    cast_dtm(id, word, n)

  lda_res <- LDA(DocumentTermMatrix, k = selected_number_topics, control = list(seed = selected_seed))
  topic_word_density <- tidy(lda_res, matrix = "beta")


  top_words_per_topic <- topic_word_density %>%
    group_by(topic) %>%
    top_n(25, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  plot_topic_word_density <- top_words_per_topic %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()

  newlist <- list(top_words_per_topic = top_words_per_topic,plot_topic_word_density = plot_topic_word_density)

  return(newlist)

}









run_sentiment <- function(clean_data,corpus_name,response_name,selected_ngram,selected_seed,selected_burning,selected_number_topics){
  
  
  bing <- get_sentiments("bing") 
  nrc <- get_sentiments("nrc")
  
  
  #sentiment comparison word cloud - PLOT 1
  
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
                   title.size = 0.75, max.words = 250,
                   scale = c(1.5, 0.4), rot.per = 0.325)
  
  
  
  
  
  #Sentiment Frequency per response - PLOT 2
  
  
  sentiment.orientation <- data.frame(orientation = c(rep("Positive",5),rep("Negative",5)),
                                      sentiment = c("anticipation","joy","positive","trust","surprise","anger","disgust","fear","negative","sadness"))
  
  
  nrc_sent <- clean_data %>%
    unnest_tokens(word, !!sym(corpus_name)) %>%
    count(!!sym(response_name),word) %>%
    group_by(!!sym(response_name)) %>%
    inner_join(nrc ,by=c("word")) %>%
    group_by(!!sym(response_name),sentiment) %>%
    summarize(total=sum(n))
  

  sentiment_freq_per_response <- nrc_sent %>%
    ggplot(aes(x=sentiment,y=total,fill=sentiment, group = !!(sym(response_name)))) + geom_bar(stat='identity',position='dodge')+ 
    #facet_wrap(~(!!sym(response_name)), scales='free') +
    ylab('Word Count') +
    xlab('Sentiment') +
    labs(title='Sentiment Frequency per Response') + 
    theme(axis.text.x  = element_text(angle=90))
  
  
  
  # Average Sentiment per Response - PLOT 3
  
  sentr_sent <- clean_data %>%
    get_sentences() %>%
    sentiment_by(by = as.character(response_name))
  
  average_sentiment_per_response <- ggplot(sentr_sent, aes(x = reorder(!!sym(response_name), ave_sentiment), y = ave_sentiment, fill = !!sym(response_name))) + 
    geom_bar(stat= 'identity') + labs(x = 'Response', y = 'Average Sentiment', title = "Average Sentiment per Response")
  
  
  
 
  
  #positive/negative sentiment distribution per response - PLOT - 4 
  
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
    geom_bar(stat = 'identity', position = 'dodge') + labs(y = 'Sentiment Percentage', title = 'Sentiment Distribution per Response')
  

  
  
  
  
  newlist <- list(comparisioncloud_per_sentiment = t_t[1:5],
                  sentiment_freq_per_response = sentiment_freq_per_response,
                  average_sentiment_per_response = average_sentiment_per_response,
                  pos_neg_sentiment_per_response = pos_neg_sentiment_per_response)
  
  return(newlist)
  
  
  
  
  
  
}










