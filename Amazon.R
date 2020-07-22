
library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/HP-15-au623tax-15-6-inch-Integrated-Graphics/product-reviews/B06XVF28NC/ref=dpx_acr_txt?showViewpoints=1&pageNumber"
i=1
p=1

amazon_reviews <- NULL
while(p>0){
  t_url <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- t_url %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
  i <- i+1
  p=length(rev)
}

length(amazon_reviews)
setwd("C:\\Users\\SD\\Desktop")
write.table(amazon_reviews,"HP.txt",row.names = F)

HP <- read.delim(choose.files())
str(HP)

#Build Corpus
library(tm)
corpus <- iconv(HP$x, to = 'utf-8')
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean text
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeWords,stopwords('english'))
inspect(corpus[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
corpus <- tm_map(corpus,content_transformer(removeURL))
inspect(corpus[1:5])

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

corpus <- tm_map(corpus,gsub,pattern = 'laptops', replacement='laptop') #Removing the common word laptop
inspect(corpus[1:5])

#Term document matrix
tdm <- TermDocumentMatrix(corpus)
tdm

tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

#Bar plot
w <- rowSums(tdm)
w <- subset(w,w>=3)
barplot(w,las=2,col=rainbow(50))

#Wordcloud
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)

#Building the wordcloud
wordcloud(words = names(w),freq = w)
#Designing the wordcloud
wordcloud(words = names(w),freq = w,max.words = 300,random.order = F,
          min.freq = 2,colors = brewer.pal(8,'Dark2'),
          scale = c(3,0.5),rot.per = 0.7)

#Sentiment analysis
install.packages("syuzhet")
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)

#Obtain sentiment scores
corpus1 <- iconv(HP$x, to ='utf-8')
s <- get_nrc_sentiment(corpus1)
head(s)

#Barplot
barplot(colSums(s),las=2,col=rainbow(10),
        ylab='Count',main='Sentiment Scores for HP reviews')

#Most of the Amazon reviews about HP laptop are Positive
