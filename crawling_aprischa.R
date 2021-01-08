# APRISCHA NAUVA M

library(devtools)
library(twitteR)
library(textclean)
library(katadasaR)
library(tokenizers)
library(ROAuth)
library(RCurl)
library(plyr)
library(wordcloud)
library(tm)
library(RColorBrewer)

setwd("/Users/aprischanauva/Documents/DATMIN")

#crawling data dari twitter (tokopedia tweet)
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

CUSTOMER_KEY <- "XXXXXXXXXXXXXXXXXXXXX" #consumerAPIkey
CUSTOMER_SECRET <- "XXXXXXXXXXXXXXXXXXXXX" #APISecretkey
ACCESS_TOKEN <- "XXXXXXXXXXXXXXXXXXXXX" #Accesstoken
ACCESS_secret<- "XXXXXXXXXXXXXXXXXXXXX" #Accesstokensecret

setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

some_tweets <- searchTwitter('tokopedia', n = 2000, lang="id")
