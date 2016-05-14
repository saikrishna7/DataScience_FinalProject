#install necessary packages and load them
install.packages("twitteR")
library(twitteR)
#read the input data file
chicago_small <- read.csv("D:/datasets/Data Sets/data science final project/chicago_small.csv")

#eliminating column which is just an unnecessary index
chicago_small=chicago_small[,2:15]

#assign column names to dataframe
names(chicago_small)=c('created_at','text','from_user','from_user_id_str','from_user_name','from_user_followers','from_user_following','from_user_favorites','from_user_tweets','from_user_timezone','to_user','to_user_id_str','to_user_name','source')

#eliminating from_user column as it is same as from_user_id_str column
chicago_small=chicago_small[c(1,2,4,5,6,7,8,9,10,11,12,13,14)]

#Check to see who has highest number of followers
which(chicago_small$from_user_followers==max(chicago_small$from_user_followers))
chicago_small[4092,]
#Check to see who is following highest number of twitter users
which(chicago_small$from_user_following ==max(chicago_small$from_user_following))
chicago_small[3404,]
#check to see who has highest number of tweets
which(chicago_small$from_user_tweets ==max(chicago_small$from_user_tweets))
chicago_small[1116,]


#check to see highets number of favourites for person who is tweeting
which(chicago_small$from_user_favorites ==max(chicago_small$from_user_favorites))
chicago_small[35]

install.packages('gdata')
library(gdata)
library(stringr)
library(plyr)
library(ggplot2)
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
Corpustweets=Corpus(VectorSource(chicago_small$text))
#Converting corpus to lower case
Corpustweets=tm_map(Corpustweets,tolower)
#Removeing punctuations from corpus
Corpustweets=tm_map(Corpustweets,removePunctuation)
#removing stopwords
Corpustweets=tm_map(Corpustweets,removeWords,c('chicago','Chicago',stopwords("english")))
Corpustweets <- tm_map(Corpustweets, stripWhitespace)
Corpustweets <- tm_map(Corpustweets, trim)


#breakk                         
Corpustweets <- tm_map(Corpustweets, PlainTextDocument)

Corpustweets = tm_map(Corpustweets, stemDocument)

#poltting first word cloud
library(wordcloud)
pdf("sparse_wordcloud.pdf")
print(wordcloud(Corpustweets))
dev.off()


#finding frequncies
frequencies=DocumentTermMatrix(Corpustweets)
frequencies
Sparsetweets=removeSparseTerms(frequencies,0.995)
Sparsetweets=as.data.frame(as.matrix(Sparsetweets))
head(Sparsetweets)
colnames(Sparsetweets) = make.names(colnames(Sparsetweets))

names(chicago_small)

#Convert Twitter dates to POSIXct objects
chicago_small$created_at <- as.POSIXct(strptime(chicago_small$created_at, "%Y %M %D %H:%M:%S", tz = "GMT"))


install.packages('lubridate')
library(lubridate)
library(scales)

chicago_small$created_at <- ymd_hms(chicago_small$created_at)
chicago_small$created_at <- with_tz(chicago_small$created_at, "America/Chicago")

#Tweets by Year, Month, and Day
ggplot(data = chicago_small, aes(x = created_at)) +
  geom_histogram(aes(fill = ..count..)+bins = 100) +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


ggplot(data = chicago_small, aes(x = created_at)) +
  geom_histogram(aes(fill = ..count..),binwidth = 100)


















#Building a CART model
library(rpart)
library(rpart.plot)

tweetCART = rpart(Class ~ ., data=Sparsetweets, method="class")
pdf("CARTtree.pdf")
print(prp(tweetCART))
dev.off()

# Making prediction
predictCART = predict(tweetCART,type="class")

table(Sparsetweets$Class[1:330], predictCART)
print("CART accuracy")
sum(diag(as.matrix(table(Sparsetweets$Class[1:330], predictCART))))/330



# Evaluate the performance of the model
predictCART = predict(tweetCART,newdata=Sparsetweets[331:344,],type="class")

table(Sparsetweets$Class[331:344], predictCART)
predictCART
#Random forest
library(randomForest)
set.seed(123)

tweetRF = randomForest(Class ~ ., data=Sparsetweets[1:330,],method="class")

# Make predictions:
predictRF= predict(tweetRF,method="class")
table(SparseTweets$Class[1:330],predictRF)
print("RF accuracy")
sum(diag(as.matrix(table(Sparsetweets$Class[1:330], predictRF))))/330

#Evaluating performance
predictRF = predict(tweetRF, newdata=Sparsetweets[331:344,],method="class")
predictRF                    

