# APRISCHA NAUVA M

library(tm)
library(NLP)
library(dbscan)
library(stringr)
library(ROAuth)
library(proxy)
library(colorspace)
library(twitteR)
library(caret)
library(dplyr)
library(katadasaR)
library(devtools)
library(tau)
library(parallel)
library(tokenizers)
library(wordcloud)
library(textclean)
library(factoextra) 
library(NbClust) 
library(readr)

setwd("/Users/aprischanauva/Documents/manjah")

Tweet <- read_csv("~/Documents/DATMIN/rawraw.csv")

some_txt = sapply(unlist(some_tweets), function(x) '$'(x, "text"))

myCorpus <- Corpus(VectorSource(some_tweets$text))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
try.error = function(x)
{
  y=NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, "error"))
    y=tolower(x)
  return(y)
}
some_txt = sapply(some_txt, try.error)
some_txt = sapply(some_txt, try.error)
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
View(some_txt)

some_txt <- some_txt %>% 
  as.data.frame() %>% 
  distinct()

nrow(some_txt)

some_txt <- as.character(some_txt$.)
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

library(tm)
stopwords <- read.csv("/Users/aprischanauva/Documents/manjah/singkatan-lib.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)
stopwords <- c(stopwords, stopwords())

some_txt <- VectorSource(some_txt)
some_txt <- VCorpus(some_txt)
some_txt <- tm_map(some_txt, content_transformer(tolower))
some_txt <- tm_map(some_txt, removeWords, stopwords)
some_txt <- tm_map(some_txt, stripWhitespace)

some_txt[[1]]$content
View(some_txt)

dtm <- TermDocumentMatrix(some_txt)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)
tail(d, 10)

inspect(dtm)
f <- findFreqTerms(dtm, 30)
f
class(f)
inspect(removeSparseTerms(dtm, 0.4))

dev.new(width = 10000000, height = 10000000, unit = "px")

#barplot(d[1:10,]$freq, las = , 
#  names.arg = d[1:10,]$word,
# col ="lightblue", main ="Most frequent words",
#  ylab = "Word frequencies")

wordcloud (some_txt, scale=c(5,0.5), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

library(tictoc)
tic()
row_total = apply(dtm, 1, sum)
dtm.new = dtm[row_total>0,]
toc()

tdm2 <- dtm[-c(2,4,19,28,29,30,31,39,40,41,49,73,91,100,101,110,164,194,201,220,250,270,289,292,293,325,338,374,383,389,401,404,408,412,435,444,461,464,481,482,507,538,560,568,577,580,582,586,594,598,609,641,647,651,670,672,678,687,702,707,712,717,723,730,739,740,749,759,763,764,769,818,824,844,869,874,879,897,909,931,980,992,1024,1028,1046,1047,1051,1061,1066,1075,1095,1102,1116,1131,1143),]
tdm2

tdm <- tm::DocumentTermMatrix(some_txt) 
tdm.tfidf <- tm::weightTfIdf(tdm2)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

#kmeans – determine the optimum number of clusters (elbow method)
#look for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:20
for (i in 2:20) wss[i] <- sum(kmeans(tfidf.matrix,centers=i,nstart=25)$withinss)
dev.new(width = 10000000, height = 10000000, unit = "px")
plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

k2 <- kmeans(tfidf.matrix, 3)
print(k2)
