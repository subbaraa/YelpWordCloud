yelprestaurants<-function(link)
{
  source=url(link)
  thepage=readLines(source)
  pattern<-"(itemprop=\"description\" lang=\"en\")"
  reviews<-grep(pattern, thepage)
  reviews_text<-matrix(nrow=length(reviews), ncol=1)
  for(i in 1:length(reviews))
  {
    temp_text <-thepage[reviews[i]]
    temp_text <-strsplit(temp_text, "\">")[[1]][2]
    temp_text<-strsplit(temp_text, "</p>")[[1]][1]
    reviews_text[i]<-temp_text
  }
  concat_text <- reviews_text[1]
  for (i in 2:nrow(reviews_text))
  {
    concat_text<-paste(concat_text, reviews_text[i])
  }
  corp1<-Corpus(VectorSource(concat_text))
  corp1<-tm_map(corp1, stripWhitespace)
  corp1<-tm_map(corp1, content_transformer(tolower))
  corp1<-tm_map(corp1, removeWords, stopwords("english"))
  corp1<-tm_map(corp1, removePunctuation)
  corp1<-tm_map(corp1, removeNumbers)
  dtm <- DocumentTermMatrix(corp1)
  dtm2<-as.matrix(dtm)
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing=TRUE)
  words <- names(frequency)
  print(frequency[1:100])
  wordcloud(words[1:30], frequency[1:30], scale=c(5,.5), rot.per=.1, use.r.layout=FALSE, color=brewer.pal(8,"Dark2"))
}
