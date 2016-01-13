#This script finds the most frequent terms from a Twitter handle

library(httr)
library(tm)
library(RWeka)

#Inputs 
#Pick a twitter handle
screen_name <- "nytimes"
#Pick the number of tweets to get (API limits the number of tweets to 200)
count <- "200"
#Pick the minimum ngram and the maximum ngram 
#For ex., do you just want bigrams? Then it's 2, 2. Bigrams and trigrams? 2, 3.
ngram_min = 1
ngram_max = 4


#Find OAuth settings for twitter:
#https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

#Set up credentials
key <- Sys.getenv("key")
secret <- Sys.getenv("secret")
token = Sys.getenv("token")
token_secret = Sys.getenv("token_secret")

myapp = oauth_app("twitter",
                  key=key,secret=secret)

sig = sign_oauth1.0(myapp,
                    token = token,
                    token_secret = token_secret)

#Make the API call
url <- paste("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",
             screen_name,"&count=",count,sep="")
tweets = GET(url,sig)

#Convert the data to json
tweet_text = content(tweets)

#Extract the text of the tweets
tweets = c()

for(a in tweet_text){
        print(a$text)
        tweets <- append(tweets,a$text)
}

tweets <- as.data.frame(tweets)

#Create corpus
mycorpus <- Corpus(VectorSource(tweets))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram_min, max = ngram_max))
myDtm <- TermDocumentMatrix(mycorpus, control = list(tokenize = BigramTokenizer))
#myDtm <- TermDocumentMatrix(mycorpus)
#frequents <- findFreqTerms(myDtm, 100)
sums <- apply(myDtm,1,sum)

#Find the number of terms
number_of_terms <- length(sums)
#Find the most frequent terms
frequent <- subset(sums,sums > 1)

#Sort and format
frequent <- as.data.frame(frequent)
frequent$word <- row.names(frequent)
frequent <- frequent[order(-frequent$frequent),]

