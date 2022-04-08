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
Samsung_df1 <- sapply(Samsung_tweets, function(x) x$getText())
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

Samsung_df6

Samsung_df7<- Corpus(VectorSource(Samsung_df6))
Samsung_df7<- tm_map(Samsung_df7, removePunctuation)
Samsung_df7<- tm_map(Samsung_df7, content_transformer(tolower))
Samsung_df7<- tm_map(Samsung_df7, removeWords, stopwords("english"))
Samsung_df7<- tm_map(Samsung_df7, stripWhitespace)

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")

wordcloud(Samsung_df7, min.freq = , max.words = Inf, width=1000,
          height=1000, random.order = FALSE, color= pal )
##### sentiment analysis #####
mysentiment<- get_nrc_sentiment(Samsung_df6) 
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
sentimentr_samsung <- sentiment_by(Samsung_df6, by=NULL)
ggplot(data=sentimentr_samsung,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_s <- sentimentr_samsung %>%
  sentiment_by(by=NULL)%>%
  highlight()
extract_sentiment_terms(Samsung_df6)

##### Extracting all the hashtags in the tweets ######
hash<-str_extract_all(Samsung_df[,1],"#\\S+") #### ? doesnt work anymore i dont know why
hash<-data.table::transpose(hash)                      
df<-sapply(hash, "length<-",max(lengths(hash)))
View(df)
write.csv(df,"Samsung_Hashtags.csv")

##### 

#### Apple ####
##### Preprocessing Tweets #####
Apple_proc_tweets <- sapply(Apple_tweets, function(x) x$getText())
#remove rt
Apple_proc_tweets2<- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Apple_proc_tweets)
#removelink
Apple_proc_tweets3<- gsub("http[^[:blank:]]+","",Apple_proc_tweets2)
#remove name
Apple_proc_tweets4<-gsub("@\\u+","",Apple_proc_tweets3)
#remove punctuations
Apple_proc_tweets5<-gsub("[[:punct:]]"," ",Apple_proc_tweets4)
#remove punctuations
Apple_proc_tweets6<-gsub("[^[:alnum:]]"," ",Apple_proc_tweets5)

Apple_proc_tweets6

Apple_proc_tweets7<- Corpus(VectorSource(Apple_proc_tweets6))
Apple_proc_tweets7<- tm_map(Apple_proc_tweets7, removePunctuation)
Apple_proc_tweets7<- tm_map(Apple_proc_tweets7, content_transformer(tolower))
Apple_proc_tweets7<- tm_map(Apple_proc_tweets7, removeWords, stopwords("english"))
Apple_proc_tweets7<- tm_map(Apple_proc_tweets7, stripWhitespace)

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")

wordcloud(Apple_proc_tweets7, min.freq = , max.words = Inf, width=1000,
          height=1000, random.order = FALSE, color= pal )
##### sentiment analysis #####
mysentiment_apple<- get_nrc_sentiment(Apple_proc_tweets6) 
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
sentimentr_apple <- sentiment_by(Apple_proc_tweets6, by=NULL)
ggplot(data=sentimentr_apple,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_a <- sentimentr_apple %>%
  sentiment_by(by=NULL)%>%
  highlight()
extract_sentiment_terms(Apple_proc_tweets6)

##### Extracting all the hashtags in the tweets ######
hash<-str_extract_all(Apple_df[,1],"#\\S+")
hash<-data.table::transpose(hash)                      
df_apple<-sapply(hash, "length<-",max(lengths(hash)))
View(df_apple)
write.csv(df,"Apple_Hashtags.csv")


