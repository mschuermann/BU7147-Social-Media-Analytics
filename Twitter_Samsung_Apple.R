#### importing libraries ####
library(twitteR)
library(RCurl)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentimentr)
library(syuzhet)
library(textclean)
library(tidyverse)
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

write.csv(apple_df,"Apple_df.csv")

##### Apple: Use this dataframe from now on so we work on same data #####
apple_df <- read.csv("Apple_df.csv")

###Samsung####
##### Preprocessing Tweets #####
#replace emojis with sentiment
Samsung_df1 <- samsung_df$text %>%
  str_to_lower() %>%
  replace_contraction() %>%
  replace_internet_slang() %>%
  replace_hash(replacement = "") %>%
  replace_word_elongation() %>%
  replace_emoji() %>%
  replace_emoji_identifier() %>%
  replace_non_ascii() %>%
  str_squish() %>%
  str_trim()
#remove rt
Samsung_df2<- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Samsung_df1)
#removelink
Samsung_df3<- gsub("http[^[:blank:]]+","",Samsung_df2)
#remove name
Samsung_df4<-gsub("@\\u+","",Samsung_df3)
#remove punctuations
Samsung_df5<-gsub("[[:punct:]]"," ",Samsung_df4)
#remove punctuations
Samsung_df6<-gsub("[^[:alnum:]]"," ",Samsung_df5)
#remove stop words
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
Samsung_df7 <- stringr::str_replace_all(Samsung_df6, stopwords_regex, '')
Samsung_df7

#not used -> Samsung_df7<- Corpus(VectorSource(Samsung_df6))
#not used -> Samsung_df7<- tm_map(Samsung_df7, removePunctuation)
#not used -> Samsung_df7<- tm_map(Samsung_df7, content_transformer(tolower))
#not used -> Samsung_df7<- tm_map(Samsung_df7, removeWords, stopwords("english"))
#not used -> Samsung_df7<- tm_map(Samsung_df7, stripWhitespace)

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")

wordcloud(Samsung_df7, min.freq = , max.words = 1000, width=1000,
          height=1000, random.order = FALSE, color= pal)
##### sentiment analysis #####
mysentiment<- get_nrc_sentiment(Samsung_df7) 
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
sentimentr_samsung <- sentiment_by(Samsung_df7, by=NULL)
ggplot(data=sentimentr_samsung,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_s <- sentimentr_samsung %>%
  sentiment_by(by=NULL)%>%
  highlight()
extract_sentiment_terms(Samsung_df6)

##### 

#### Apple ####
##### Preprocessing Tweets #####
#replace emojis with sentiment
Apple_df1 <- apple_df$text %>%
  str_to_lower() %>%
  replace_contraction() %>%
  replace_internet_slang() %>%
  replace_hash(replacement = "") %>%
  replace_word_elongation() %>%
  replace_emoji() %>%
  replace_emoji_identifier() %>%
  replace_non_ascii() %>%
  str_squish() %>%
  str_trim()
#remove rt
Apple_df2<- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Apple_df1)
#removelink
Apple_df3<- gsub("http[^[:blank:]]+","",Apple_df2)
#remove name
Apple_df4<-gsub("@\\u+","",Apple_df3)
#remove punctuations
Apple_df5<-gsub("[[:punct:]]"," ",Apple_df4)
#remove punctuations
Apple_df6<-gsub("[^[:alnum:]]"," ",Apple_df5)
#remove stop words
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
Apple_df7 <- stringr::str_replace_all(Apple_df6, stopwords_regex, '')
Apple_df7

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")

wordcloud(Apple_df7, min.freq = , max.words = 1000, width=1000,
          height=1000, random.order = FALSE, color= pal )
##### sentiment analysis #####
mysentiment_apple<- get_nrc_sentiment(Apple_df7) 
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
sentimentr_apple <- sentiment_by(Apple_df7, by=NULL)
ggplot(data=sentimentr_apple,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_a <- sentimentr_apple %>%
  sentiment_by(by=NULL)%>%
  highlight()
extract_sentiment_terms(Apple_df7)



