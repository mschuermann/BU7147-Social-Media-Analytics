#### importing libraries ####
library(twitteR)
library(RCurl)
library(ROAuth)
library(plyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentimentr)
library(syuzhet)
library(textclean)
library(tidyverse)
library(tidytext)
library(tm)
library(rtweet)
library(SnowballC)
library(base64enc)
library(openssl)
library(httpuv)

#### setting the working directory ####
setwd("/Users/Lily/Library/Mobile Documents/com~apple~CloudDocs/Trinity College Dublin/BU7147 Social Media Analysis/Group Assignment/Social-Media-Analytics")

#### initializing keys ####
bearer_token<-"#######"
consumer_key<-"#######"
consumer_secret<-"#######"
access_token<-"#######"
access_token_secret<-"#######"

setup_twitter_oauth(consumer_key=consumer_key,
                    consumer_secret=consumer_secret,
                    access_token=access_token,
                    access_secret=access_token_secret)


#### Getting Tweets about Samsung ####
stweets1 <- searchTwitter("#samsungs22 -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
sdf1 <- twListToDF(stweets1)
View(sdf1)

stweets2 <- searchTwitter("Samsung + S22 -@ShopeeID -@_arllee -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
sdf2 <- twListToDF(stweets2)
View(sdf2)

sd2_users <- sdf2 %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(sd2_users)
sum(sd2_users$`number of tweets`)

ad_tweets <- c("whitestonedome", "FromKorea5", "dome_glass", "Whitestone_DE", "whitestone_UK", "jp_whitestone", "Whitestone__FR", "WhitestoneJapan", "WhitestoneEU")

sdf2_filter <- sdf2 %>%
  filter(!screenName %in% ad_tweets)
View(sdf2_filter)

sd2_users2 <- sdf2_filter %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(sd2_users2)

samsung_df <- rbind(sdf1, sdf2_filter)
View(samsung_df)
write.csv(samsung_df,"Samsung_df.csv")

##### Samsung: Use this dataframe from now on so we work on same data #####
samsung_df <- read.csv("Samsung_df.csv")
View(samsung_df)


#### Getting Tweets about Apple ####
atweets1 <- searchTwitter("iPhone + 13 -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
adf1 <- twListToDF(atweets1)
View(adf1)

adf_users <- adf1 %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(adf_users)

adf_replies <- adf1 %>%
  distinct(replyToSN, id) %>%
  group_by(replyToSN) %>%
  summarise("number of replies to user" = n())
View(adf_replies)

apple_ad_tweets <- c("whitestonedome", "FromKorea5", "domeglassapple")

adf_filter <- adf1 %>%
  filter(!screenName %in% apple_ad_tweets)
View(adf_filter)

atweets2 <- searchTwitter("#iphone13 -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
adf2 <- twListToDF(atweets2)
View(adf2)

adf_users2 <- adf2 %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(adf_users2)

adf_replies2 <- adf2 %>%
  distinct(replyToSN, id) %>%
  group_by(replyToSN) %>%
  summarise("number of replies to user" = n())
View(adf_replies)

apple_df <- rbind(adf_filter, adf2)

#there is another competition which did not need retweets or replying to someone specifically 
#people copied the text to take part, therefore, the word "treasure" and "win" were exorbitant in our analysis
#therefore we excluded all rows that had this text
apple_df <- apple_df %>%
  filter(!grepl("Join the event to win an iPhone 13!",text)) 
View(apple_df_excl)

write.csv(apple_df,"Apple_df.csv")

##### Apple: Use this dataframe from now on so we work on same data #####
apple_df <- read.csv("Apple_df.csv")

###Samsung####
##### Preprocessing Tweets #####
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')

Samsung_df <- samsung_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  replace_emoji() %>% #replaces emojis with the word form 
  replace_emoji_identifier() %>% #replaces emoji identifiers to word form 
  replace_non_ascii() %>% #replaces common non-ASCII characters. 
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  stringr::str_replace_all(stopwords_regex, '')#remove stop words

##### sentiment analysis #####
mysentiment<- get_nrc_sentiment(Samsung_df)
sentimentscores<- data.frame(colSums(mysentiment[,]))

###### getting sentiment scores ######
names(sentimentscores)<-"score"
sentimentscores<-cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores)<-NULL

###### plotting sentiment scores ######
ggplot(data=sentimentscores,aes(x=sentiment,y=score))+
  geom_bar(aes(fill=sentiment),stat="identity")+ 
  theme(legend.position = "none")+
  xlab("sentiment") +ylab("score")+ ggtitle("total sentiment score based on tweets about Samsung")

##### Sentimentr score ######
sentimentr_samsung <- sentiment_by(Samsung_df, by=NULL)
ggplot(data=sentimentr_samsung,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_s <- sentimentr_samsung %>%
  sentiment_by(by=NULL)%>%
  highlight()
extract_sentiment_terms(Samsung_df)

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')

word_s <- samsung_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  #replace_emoji() %>% #replaces emojis with the word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_emoji_identifier() %>% #replaces emoji identifiers to word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_non_ascii() %>% #replaces common non-ASCII characters. #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  stringr::str_replace_all(stopwords_regex, '')#remove stop words

wordcloud(word_s, min.freq = , max.words = 1000, width=1000,
          height=1000, random.order = FALSE, color= pal)

##### Creating a plot of the positive words most frequently used #####
bg_df_s <- data.frame(word_s)

s_bigram <- bg_df_s %>%
  unnest_tokens(output=bigrams, input=word_s, token="words", format= "text")

s_bigram_counted <- s_bigram %>% count(bigrams, sort = TRUE)
s_bigram_counted$sentiment <- get_sentiment(s_bigram_counted$bigrams)

ggplot(s_bigram_counted, aes(x=bigrams, y=n))+
  geom_col()
s_bigram_with_sentiment <- s_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  top_n(25)

ggplot(s_bigram_with_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()

##### Creating a plot of the negative words most frequently used #####

s_bigram_with_neg_sentiment <- s_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  tail(25)

ggplot(s_bigram_with_neg_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()

#### Apple ####
##### Preprocessing Tweets #####
#replace emojis with sentiment
Apple_df <- apple_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  replace_emoji() %>% #replaces emojis with the word form 
  replace_emoji_identifier() %>% #replaces emoji identifiers to word form 
  replace_non_ascii() %>% #replaces common non-ASCII characters. 
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  stringr::str_replace_all(stopwords_regex, '')#remove stop words

##### sentiment analysis #####
mysentiment_apple<- get_nrc_sentiment(Apple_df) 
sentimentscores_apple<- data.frame(colSums(mysentiment_apple[,]))

###### getting sentiment scores ######
names(sentimentscores_apple)<-"score"
sentimentscores_apple<-cbind("sentiment"=rownames(sentimentscores_apple),sentimentscores_apple)
rownames(sentimentscores_apple)<-NULL

###### plotting sentiment scores ######
ggplot(data=sentimentscores_apple,aes(x=sentiment,y=score))+
  geom_bar(aes(fill=sentiment),stat="identity")+ 
  theme(legend.position = "none")+
  xlab("sentiment") +ylab("score")+ ggtitle("total sentiment score based on tweets about Apple")

###### Sentimentr score #######
sentimentr_apple <- sentiment_by(Apple_df, by=NULL)
ggplot(data=sentimentr_apple,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_a <- sentimentr_apple %>%
  sentiment_by(by=NULL)%>%
  highlight()

sentiments_a <- get_sentences(Apple_df) %>%
  extract_sentiment_terms()


##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")

word_a <- apple_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  #replace_emoji() %>% #replaces emojis with the word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_emoji_identifier() %>% #replaces emoji identifiers to word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_non_ascii() %>% #replaces common non-ASCII characters. #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  stringr::str_replace_all(stopwords_regex, '')#remove stop words

wordcloud(word_a, min.freq = , max.words = 1000, width=1000,
          height=1000, random.order = FALSE, color= pal)

##### Creating a plot of the positive words most frequently used #####

bg_df_a <- data.frame(word_a)

a_bigram <- bg_df_a %>%
  unnest_tokens(output=bigrams, input=word_a, token="words", format= "text")

a_bigram_counted <- a_bigram %>% count(bigrams, sort = TRUE)
a_bigram_counted$sentiment <- get_sentiment(a_bigram_counted$bigrams)

ggplot(a_bigram_counted, aes(x=bigrams, y=n))+
  geom_col()
a_bigram_with_sentiment <- a_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  top_n(25)

ggplot(a_bigram_with_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()
View(apple_df)

##### Creating a plot of the negative words most frequently used #####

a_bigram_with_neg_sentiment <- a_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  tail(25)

ggplot(a_bigram_with_neg_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()


