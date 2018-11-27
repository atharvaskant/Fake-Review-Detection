#read csv file
mydata = read.csv("C:\\Users\\athar\\Desktop\\FYP\\MyDataFinal.csv", sep=",")
#remove duplicates
mydata=unique(mydata)

#include library for cleaning of data
library(tm)

#Taking 5000 entries as test data
testdata<-mydata[sample(nrow(mydata), size=5000, replace=FALSE),]

REVIEW = testdata$Review

#create data object
data <- as.data.frame(REVIEW)

#write to csv file
write.csv(data,file="C:\\Users\\athar\\Desktop\\FYP\\Test.csv",row.names=F)

#make corpus
iconv(data, from= "latin1", to="ASCII", sub="")
docs <- Corpus(VectorSource(data$REVIEW))

#view corpus
inspect(docs)

docs <- tm_map(docs, removePunctuation)

#remove special characters
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("#", " ", docs[[j]])
  docs[[j]] <- gsub("&", " ", docs[[j]])
  docs[[j]] <- gsub("$", " ", docs[[j]])
  docs[[j]] <- gsub(";", " ", docs[[j]])
  docs[[j]] <- gsub("!", " ", docs[[j]])
  docs[[j]] <- gsub("%", " ", docs[[j]])
}

docs <- tm_map(docs, removeNumbers)  
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))

#inspect(docs)

library(SnowballC)
docs <- tm_map(docs, stripWhitespace)

#create term document matrix and sort terms in increasing order of frequency
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

#Cloud Word  

library(wordcloud)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


     




