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
library(SnowballC)
library(base64enc)
library(openssl)
library(httpuv)

#### setting the working directory ####
setwd("/Users/Lily/Library/Mobile Documents/com~apple~CloudDocs/Trinity College Dublin/BU7147 Social Media Analysis/Group Assignment")

#### initializing keys ####
bearer_token<-"AAAAAAAAAAAAAAAAAAAAADAsZwEAAAAApJZG862%2F2iV6P3ap%2BqLLhX0cCVU%3DUMSikoIxGoqjnRdOWbJFpTyQxKmm9TNVduC45wpUd1gf8Md8ma"
consumer_key<-"yxWBMj9vqek0xiIpwvjpLqbOH"
consumer_secret<-"bnlb6ANaokXQpms2ZDb46wIRGVWHuB0R22SBljL5LoVeTTdiC9"
access_token<-"1499053441615900680-ULvq2UGGBinckDD7p81GALBXgEl8or"
access_token_secret<-"PRTPXdgxTNpcpHl9rzlP6Kr3DyZ0XkFexaQHg98ZRrUs8"

setup_twitter_oauth(consumer_key=consumer_key,
                    consumer_secret=consumer_secret,
                    access_token=access_token,
                    access_secret=access_token_secret)

#### Getting Tweets about Samsung ####
Samsung_tweets <- searchTwitter("@Samsung -filter:retweets", n=1000, lang="en", since="2022-03-19", until="2022-03-29", retryOnRateLimit = 100)
Samsung_df <- twListToDF(Samsung_tweets)
View(Samsung_df)
write.csv(Samsung_df,"Samsung_df_03_2022.csv")

##### Samsung: Use this dataframe from now on so we work on same data #####
Samsung_df <- read.csv("Samsung_df_03_2022.csv")

#### Getting Tweets about Apple ####
Apple_tweets <- searchTwitter("@Apple -filter:retweets", n=1000, lang="en", since="2022-03-19", until="2022-03-29", retryOnRateLimit = 100)
Apple_df <- twListToDF(Apple_tweets)
View(Apple_df)
write.csv(Apple_df,"Apple_df_03_2022.csv")

##### Apple: Use this dataframe from now on so we work on same data #####
Apple_df <- read.csv("Apple_df_03_2022.csv")

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


