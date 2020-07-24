library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(pdftools)
PWC<-read_pdf(file='/Users/macbookpro/Desktop/TEXT ANALYTICS/PWC REPORT.pdf')
View(PWC)
mode(PWC)


a <- 136
b <- 1
my_df<- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- PWC$text[i*b+z-b]
  }
}

PWC_reloaded <- my_df %>% ##   names the avengers with a new variable that we call after tokenzing which is avengers_reloaded for this case 
  unnest_tokens(word, V1) %>%         #tokenizing  seperates word per word that appears in the dataset
 # anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
PWC_reloaded

library(wordcloud2)
PWC_reloaded %>%
  top_n(100) %>%
  wordcloud2::wordcloud2()

library(ggplot2)
library(scales)

install.packages(("reshape2"))
library(wordcloud)
library(reshape2)
PWC_reloaded %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=50)


PWC_sentiment <- PWC_reloaded %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup()

PWC_sentiment %>%
  group_by(sentiment) %>%
  filter(n>30) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


# mode(PWC_reloaded)
big_die<-as.character( big_die)
big_die <- PWC_reloaded %>%
  unnest_tokens( bigrams,text, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

PWC_bigrams <- my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts 




# ############################################################################################
KMPG <-read_pdf(file='/Users/macbookpro/Desktop/TEXT ANALYTICS/integrated-report-2017-2018.pdf')

View(KMPG)

mode(KMPG)

a <- 233
b <- 1
my_df<- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- KMPG$text[i*b+z-b]
  }
}


#######################SECOND DATAFRAME#####################
KMPG_reloaded <- my_df %>% ##   names the avengers with a new variable that we call after tokenzing which is avengers_reloaded for this case 
  unnest_tokens(word, V1) %>%         #tokenizing  seperates word per word that appears in the dataset
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
KMPG_reloaded

library(wordcloud2)
KMPG_reloaded %>%
  top_n(100) %>%
  wordcloud2()

library(ggplot2)
library(scales)

# install.packages(("reshape2"))
library(wordcloud)
library(reshape2)
KMPG_reloaded %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=50)


KMPG_sentiment <- KMPG_reloaded %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup()

KMPG_sentiment %>%
  group_by(sentiment) %>%
  filter(n>30) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
KMPG_sentiment




library(dplyr)
library(tidytext)
# library(janeaustenr)
library(tidyr)

PWC$text <- as.character(PWC$text)


my_bigrams <- PWC %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

PWC_bigrams <- my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts



library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>13) %>%
  graph_from_data_frame()

bigram_graph

# install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label= name), vjust =1, hjust=1)








library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)

KMPG$text <- as.character(KMPG$text)


my_bigrams <- KMPG %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

KPMG_bigrams <- my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts



library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>13) %>%
  graph_from_data_frame()

bigram_graph

# install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label= name), vjust =1, hjust=1)




# ##########
book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words # we get all the zeors because we are looking at stop words ... too common

book_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=book))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~book, ncol=2, scales="free")+
  coord_flip()









