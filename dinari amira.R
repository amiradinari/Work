library('rvest')
library("tm")
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(rpart)
#web scrapping
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016'


webpage <- read_html(url)


title_data_html <- html_nodes(webpage,'.lister-item-header a')
title_data <- html_text(title_data_html)

head(title_data)

description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)


head(description_data)
description_data<-gsub("\n","",description_data)

head(description_data)



genre_data_html <- html_nodes(webpage,'span.genre')


genre_data <- html_text(genre_data_html)


head(genre_data)

genre_data<-gsub("\n","",genre_data)

genre_data<-gsub(" ","",genre_data)


genre_data<-gsub(",.*","",genre_data)


genre_data<-as.factor(genre_data)


head(genre_data)






directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

directors_data <- html_text(directors_data_html)


head(directors_data)

directors_data<-as.factor(directors_data)

actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

actors_data <- html_text(actors_data_html)


head(actors_data)
actors_data<-as.factor(actors_data)


movies_df<-data.frame( Title = title_data[1:100],Director = directors_data[1:100], Actor = actors_data[1:100],
             Genre=genre_data[1:100]         ,Description=description_data[1:100])
#text Mining
docs1 <- Corpus(VectorSource(movies_df$description_data))
inspect(docs1)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs1<- tm_map(docs1, toSpace, "/")
docs1 <- tm_map(docs1, toSpace, "@")
docs1 <- tm_map(docs1, toSpace, "\\|")
docs1=tm_map(docs1,content_transformer(tolower))
docs1=tm_map(docs1,removeNumbers)

docs1=tm_map(docs1,removeWords,stopwords("english"))
docs1=tm_map(docs1,removeWords,c("and","the","her","his","he","she","with","that","for","their"))
docs1=tm_map(docs1,removePunctuation)
docs1=tm_map(docs1,stripWhitespace)
docs1 <- tm_map(docs1, stemDocument)
dtm<- TermDocumentMatrix(docs1)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words =names(v), freq =v, min.freq = min(v),random.order=F,rot.per=.1,colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 4)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,    col ="lightblue", main ="Most frequent words",   ylab = "Word frequencies")

          
          
          
          
          
          
          
          
          
          
          
          
   





i=which(movies_df$Genre=="Action" |movies_df$Genre=="Drama" | movies_df$Genre=="Comedy"  )
docs <- Corpus(VectorSource(movies_df$Description[i]))
inspect(docs)
data<-docs
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs=tm_map(docs,content_transformer(tolower))
docs=tm_map(docs,removeNumbers)

docs=tm_map(docs,removeWords,stopwords("english"))
docs=tm_map(docs,removeWords,c("and","the","her","his","he","she","with","that","for","their"))
docs=tm_map(docs,removePunctuation)
docs=tm_map(docs,stripWhitespace)
docs=tm_map(docs,stemDocument)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(words =names(v), freq =v, min.freq = min(v),random.order=F,rot.per=.1,colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 4)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,    col ="lightblue", main ="Most frequent words",   ylab = "Word frequencies")

data<-docs
movie <- DocumentTermMatrix( data)
movie= as.matrix(movie)

movie <- as.data.frame( movie ,row.names=1:nrow(movie))

class <-movies_df$Genre[i]
movie <- cbind( movie[i], class,movies_df$Director[i],movies_df$Actor[i])
library(xlsx)
write.table(movie, "mydata.txt", sep=" ")
donné=read.table("mydata.txt")
k=sample(1:40)
train=donné[k[1:30],]
test=donné[k[31:40],]
library(rpart)
#classification

library(e1071)
svm.model <- svm(class ~ . , train, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model,newdata=test,type="class") 
ct1=table(svm.pred,test$class)
library(nnet)
for(i in 1:20)
{res.nnet<- nnet (class ~ .,data=train,skip=F,size=i,MaxNWts=12966)
pred.nnet<- predict(res.nnet,newdata=test,type="class")

ct2=table(pred.nnet,test$class)
print(ct2)}
library(rpart)

mod=rpart(class~.,data= train)
plot (mod, branch=.2, uniform=T, compress=T, margin=.1); text (mod, all=T, use.n=T, fancy=T) 
p=predict(mod,newdata=test,type="class")
ct=table(p,test$class)

