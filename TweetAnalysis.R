if(!require("tweet"))install.packages("rtweet")
if(!require("dplyr"))install.packages("dplyr")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("tidytext")) install.packages("tidytext")
if(!require("wordcloud"))install.packages("wordcloud")
if(!require("RColorBrewe"))install.packages("RColorBrewer")
if(!require("wordcloud2"))install.packages("wordcloud2")
if(!require("tm"))install.packages("tm")
if(!require("slam"))install.packages("slam")
if(!require("syuzhet"))install.packages("syuzhet")

library(RColorBrewer)
library(wordcloud2)
library(wordcloud)
library(tm)
library(slam)
library(syuzhet)
library(dplyr)
library (rtweet)
library(tidyverse)
library(tidytext)




#Twitter Dev Keys
consumerKey <-"MGVRn15j2DLuDXmimfKV0s2vs"
consumerSecret <-"Ah8a6HkFvgsWkwAS3MY7uftq8Rh6TIPUQKOwxw8H9rjVCikxtk"
accessToken <- "240674330-l6x04wZ59zJni7Aoy37H5re9raCDYPkJrZH3wuFS"
accessTokenSecret <-"zyJsBlM4SgpflawPFOYd9bUZNq0zyxaArLN2dWjIPhGvR"


#Twitter token
twitter_token <- create_token(
  app = "TweetDummyProject",
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accessToken,
  access_secret = accessTokenSecret,
  set_renv = TRUE)  

#Get the lastest 3200 tweets from Tw account
Valls <- get_timeline("@VallsAjuntament", n= 3200)

# Remove retweets
Valls_tweets_organic <- Valls[Valls$is_retweet==FALSE, ] 

# Remove replies
Valls_tweets_organic <- subset(Valls_tweets_organic, is.na(Valls_tweets_organic$reply_to_status_id)) 
Valls_tweets_organic <- Valls_tweets_organic %>% arrange(-favorite_count)
Valls_tweets_organic <- Valls_tweets_organic %>% arrange(-retweet_count)

# Keeping only the retweets
Valls_retweets <- Valls[Valls$is_retweet==TRUE,]
# Keeping only the replies
Valls_replies <- subset(Valls, !is.na(Valls$reply_to_status_id))


# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(nrow(Valls_tweets_organic),nrow(Valls_retweets), nrow(Valls_replies)
))

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

# Specify what the legend should say
# % of organic, retweets and replies from tw account
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# Number of tweets by month
colnames(Valls)[colnames(Valls)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(Valls, Twitter_Account), "month") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Valls Ajuntament",
    subtitle = "Tweet counts aggregated by month",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

##WORDS FOUND IN TWEETS
Valls_tweets_organic$text <-  gsub("https\\S*", "", Valls_tweets_organic$text)
Valls_tweets_organic$text <-  gsub("@\\S*", "", Valls_tweets_organic$text) 
Valls_tweets_organic$text  <-  gsub("amp", "",Valls_tweets_organic$text) 
Valls_tweets_organic$text  <-  gsub("[\r\n]", "", Valls_tweets_organic$text)
Valls_tweets_organic$text  <-  gsub("[[:punct:]]", "", Valls_tweets_organic$text)

tweets <- Valls_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)

stop_words<-data.frame(word=c("us","et","va","he","ha","a","bon","i","als","fins","en","des","mes","més","aquest","o","una","dels","amb","de","la","es","és","que","els","el","un","al","del","les","la","per"))
tweets <- tweets %>%
  anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Valls Ajuntament",
       subtitle = "Stop words removed from the list")


#Most used hashtags

Valls_tweets_organic$hashtags <- as.character(Valls_tweets_organic$hashtags)
Valls_tweets_organic$hashtags <- gsub("c\\(", "", Valls_tweets_organic$hashtags)
set.seed(1234)
wordcloud(Valls_tweets_organic$hashtags, min.freq=4, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# ACCOUNTS FROM WHICH MOST RETWEETS ORIGINATE
set.seed(1234)
wordcloud(Valls_retweets$retweet_screen_name, min.freq=2, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

#sentiment

# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
tweets <- gsub("[[:punct:]]","",tweets)
#removing numbers
tweets <- gsub("\\w*[0-9]+\\w*\\s*", "",tweets)


ew_sentiment<-get_nrc_sentiment((tweets),language="catalan")
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  geom_text(aes(label = Score),
            vjust = 1.5, color = "black",
            size = 5)+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

##ReplY Analisys
at_rdt <- search_tweets(
  "to:VallsAjuntament", 
  n = 5e2,
  retryonratelimit = TRUE
)

replyData <- subset(at_rdt,(at_rdt$screen_name!="VallsAjuntament"))
