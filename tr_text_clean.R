# Ayse Giz Gulnerman
# Disaster domain based filtering on social media: A text mining approach to remove noisy content over sentimental and information mapping
# Turkish Text Clean Function source code

#removeemoticons
removeemoticons <- function(x) {
  x <- iconv(x, 'UTF-8', 'macturkish', sub="")
  x <- iconv(x, 'macturkish', 'UTF-8', sub="")
  return(x)
}

# remove short & long words
library(ggrepel)
removeLongWords<- function(x) gsub("\\b[[:alpha:]]{13,}\\b", "", x)
removeShortWords<-function(x) {
  x <- rm_nchar_words(x, "1,1")
  return(x)
}

#remove url
library(tm, tmap)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

#stopwords list scan
# replace the file path with the download path of the 
# original data from : https://github.com/ahmetax/trstop
stopTR<- scan('stopTR.txt', what='character')

# File path is pointing to the admin names list
# source : https://www.nvi.gov.tr/PublishingImages/Pages/il-ilce-kod-tablosu/IL_ILCE_LISTESI.xls
IL_ILCE<-read.xlsx(file = "IL_ILCE_LISTESI.xlsx", header = TRUE,sheetName = "Sheet1")

# data tidying
il<- IL_ILCE$İL.ADI
ilce<-IL_ILCE$İLÇE.ADI
il<-tolower(il)
ilce<-tolower(ilce)

#remove extra punctuations
removec1<- function(x) gsub("…","",x)
removec2<- function(x) gsub("–","",x)
removec3<- function(x) gsub("’","",x)
removec4<- function(x) gsub("“","",x)
removec5<- function(x) gsub("•","",x)
removec6<- function(x) gsub("‘","",x)
removec7<- function(x) gsub("”","",x)

#turkish text clean function
tr_clean_corp<-function(corpus) {
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(removec1))
  corpus <- tm_map(corpus, content_transformer(removec2))
  corpus <- tm_map(corpus, content_transformer(removec3))
  corpus <- tm_map(corpus, content_transformer(removec4))
  corpus <- tm_map(corpus, content_transformer(removec5))
  corpus <- tm_map(corpus, content_transformer(removec6))
  corpus <- tm_map(corpus, content_transformer(removec7))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), stopTR, il, ilce, "turkey", "turkiye", "türkiye" ))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(removeLongWords))
  corpus <- tm_map(corpus, content_transformer(removeShortWords))
  corpus <- tm_map(corpus, content_transformer(removeURL))
  corpus <- tm_map(corpus, content_transformer(removeemoticons))
  return(corpus)
}