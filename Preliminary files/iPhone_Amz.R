#Social Media Analytics 
library(readxl)
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
library(base64enc)

########################################################################################

#Iphone review of Amazon data

#Set working directory
setwd("/Users/mohit/Desktop/Trinity College/BA- Semester 2/Social Media Analytics/Group Assignment/Final Amazon Dataset/Amazon iPhone reviews")
#read data
iphone.reviews <- read.csv("iPhone_Amz.csv")
#View(iphone.reviews)

dim(iphone.reviews)
summary(iphone.reviews)

# Load "Stop Words" from the tidytext package
data("stop_words")

iphone.reviews.text <- iphone.reviews %>%
  select(review_text)

# Encoding
# Check Encoding and Make it consistent
library(stringi)
stri_enc_mark(iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- sapply(iphone.reviews.text$review_text,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))

# Lowecase all text
iphone.reviews.text$review_text <- tolower(iphone.reviews.text$review_text)

# make wasn't=was not, can't=can not, etc..
iphone.reviews.text$review_text <- gsub("wasn[\u2019']t", "was not", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("won[\u2019']t", "will not", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("can[\u2019']t", "can not", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("didn[\u2019']t", "did not", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("don[\u2019']t", "do not", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("I[\u2019']m", "I am", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("[\u2019']ve", " have", iphone.reviews.text$review_text) 
iphone.reviews.text$review_text <- gsub("[\u2019|']s", "", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("[\u2019']re", " are", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("[\u2019']ll", " will", iphone.reviews.text$review_text)


# If you view common typos during your analysis, fix them here.
iphone.reviews.text$review_text<- gsub("canceling", "cancelling", iphone.reviews.text$review_text)
iphone.reviews.text$review_text <- gsub("cancellation", "cancelling", iphone.reviews.text$review_text)

# omit the following two lines if you have not loaded the tm package
# Remove numbers in the text
#iphone.reviews.text$review_text <- removeNumbers(iphone.reviews.text$review_text)

#library(NLP)
#library(tm)
# Remove punctuations in the text
iphone.reviews.text$review_text <- removePunctuation(iphone.reviews.text$review_text)

# Fix Negations
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                    "no",
                    "without",
                    "never",
                    "bad",
                    "none",
                    "never",
                    "nobody",
                    "nowhere",
                    "neither",
                    "nothing"
)

library(tidyr)
library(janeaustenr)
library(tidytext)
shifted.words <- iphone.reviews.text %>%
  unnest_tokens(bigram, review_text, token = "ngrams", n = 2)%>%
  dplyr::count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(word1 %in% negation.words & !word2 %in% stop_words$word)%>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))%>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  mutate(score = sentiment * n) %>%
  mutate(word2 = reorder(word2, score))

shifted.words

# Pick the most effective sentiment shifters
negated.phrases <- c("not worth", 
                     "not noise",
                     "no issues",
                     "no complaints",
                     "not disappoint",
                     "not disappointed",
                     "not cheap",
                     "no regrets"
                     
)

# Find synonyms for the phrases above to replace
synonyms <- c("expensive",
              "functional",
              "cool",
              "satisfied",
              "satisfied",
              "satisfied",
              "expensive",
              "satisfied"
)
library(textclean)
# Replace the negations with their synonyms.
iphone.reviews.text <- mgsub(iphone.reviews.text$review_text, negated.phrases, synonyms) %>%
  dplyr::as_data_frame() %>%
  rename(review_text = value)

# if you want to ignore words that are frequent but doesn't help, add them to this list. ###Ignore the words
ignore.words <- data_frame(word = c("iphone", "phone","apple", "13"))

# create the words freq table
word.freq.table<- iphone.reviews.text %>% 
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  dplyr::count(word, sort = TRUE)
word.freq.table

# Plotting a Wordcloud
word.freq.table %>% 
  filter(n>8) %>%
  with(wordcloud(word, n,
                 scale = c(5,0.3),
                 colors = brewer.pal(8, "Dark2")))

# Most common Positive and Negative words using Bing
iphone.reviews.text %>% 
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(percent = round(n/sum(n), 3)) %>%
  ggplot(aes(x = word, y = percent, fill = sentiment, label = percent)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  geom_text(aes(y = 0.7*percent)) +
  labs(title = "iPhone 13 Word Polarity (bing)") +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Bing
bing.mean.score <- word.freq.table %>% 
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  summarise(mean = mean(sentiment))

library(scales)
# rescale the range to 5 star range.
bing.mean.score<-rescale(bing.mean.score$mean, to = c(1,5), from = c(-1,1))

# Afinn scores are from -5 to 5.
afinn.mean.score <- word.freq.table %>% 
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean = mean(value))

# rescale the range to 5 star range.
afinn.mean.score<-rescale(afinn.mean.score$mean, to = c(1,5), from = c(-5,5))

# Correlation Terms
# The correlation of appearing together in a review
bose.correlation.terms <- iphone.reviews.text %>%
  mutate(review = row_number()) %>%
  unnest_tokens(word, review_text) %>%
  filter(!word %in% stop_words$word) %>%
  group_by(word) %>%
  filter(n() >= 5)%>%
  pairwise_cor(word, review, sort = TRUE)
bose.correlation.terms

library(ggraph)
library(igraph)
bose.correlation.terms %>%
  filter(correlation >= 0.50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "igraph", algorithm = "kk") +
  geom_edge_link(aes(alpha = correlation), 
                 show.legend = FALSE)+
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

bigrams.network.df<-iphone.reviews.text %>%
  unnest_tokens(bigram, review_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  dplyr::count(word1, word2, sort = TRUE) %>%
  filter(n > 2)

bigrams.network <- graph_from_data_frame(bigrams.network.df)

# now we find the centrality measures of the network
# degree:the number of its adjacent edges (measure of direct influence)
deg <- degree(bigrams.network, mode = "all")

#K-core decomposition allows us to identify the core and the periphery of the network. A k-core is a maximal subnet of a network such that all nodes have at least degree K.
core <- coreness(bigrams.network, mode = "all")

# betweenness measures brokerage or gatekeeping potential. It is (approximately) the number of shortest paths between nodes that pass through a particular node.
betw <- betweenness(bigrams.network)

#Eigenvector centrality is a measure of being well-connected connected to the well-connected. First eigenvector of the graph adjacency matrix. Only works with undirected networks.
eigen <- eigen_centrality(bigrams.network, directed = TRUE)
members <- cluster_walktrap(bigrams.network)

library(igraph)
bigrams.network <- simplify(bigrams.network, 
                            #remove.multiple = FALSE, #error occured ?
                            #remove.loops = TRUE)
)
V(bigrams.network)$color <- members$membership+1

# Using "Coreness" as size
# Coreness -> mean (average distance to all the other nodes, diffusion of information)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.dist = 0,
     vertex.frame.color = 0,
     vertex.size = core*10, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "gray",
     main = "Bigram Communities (iPhone 13)"
)
mtext("Coreness")

# Using "Degree" as size
# degree=mode (number of edges of the node, in-degree:prestige

plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.dist = 0,
     vertex.frame.color = 0,
     vertex.size = deg, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "gray",
     main = "Bigram Communities (iPhone 13)"
)
mtext("Degree")

# Using "Eigenvector Centrality" as size
# centrality (the most connected words)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,
     vertex.size = eigen$vector*20, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "black",
     main = "Bigram Communities (iPhone 13)"
)
mtext("Eigenvector Centrality")

# Using "Betweenness" as size
#Betweenness -> median (weighted # of paths going through the node)
plot(bigrams.network,
     layout = layout_with_fr,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,
     vertex.size = betw, 
     edge.arrow.size = 0.01,
     edge.curved = 0.7,
     edge.color = "lightgrey",
     main = "Bigram Communities (iPhone 13)"
)
mtext("Betweenness")




