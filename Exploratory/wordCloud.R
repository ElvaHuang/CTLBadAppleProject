
##--  Word Cloud  --##

rm(list=ls())
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

load("ms_level.Rda")
load("conv_level.Rda")


## paste messages together

# function to concatenate messages together
pastems <- function(dat){
  paste0(dat$message,collapse=" ")
}

# test 
# pastems(ms[1:10,])

## apply to only counselor message
require(plyr)
ms.c <- ddply(ms[ms$type=="counselor",],.(conv_id),pastems)
conv_id <- ms.c[[1]]


## Playing with the tm package 
require(tm)

## counselor 
ms.c <- VCorpus(DataframeSource(ms.c))
ms.c <- tm_map(ms.c,stripWhitespace)
ms.c <- tm_map(ms.c,content_transformer(tolower))
ms.c <- tm_map(ms.c,removeWords,stopwords("english"))
ms.c <- tm_map(ms.c,stemDocument)

tdm.c <- TermDocumentMatrix(ms.c)  # for plotting
tdm.c.r <- removeSparseTerms(tdm.c,0.8)

#findAssocs(dtm.c,"alon",0.2)
#inspect(TermDocumentMatrix(ms.c,list(dictionary = c("alon","support","wrong"))))



## playing with the word cloud

require(wordcloud)

# What the counselor is saying
m.c <- as.matrix(tdm.c.r)
v.c <- sort(rowSums(m.c),decreasing=T)
words.c <- names(v.c)
d.c <- data.frame(word=words.c,freq=v.c)

display.brewer.all()
color <- brewer.pal(12,"Set3")

wordcloud(d.c$word,d.c$freq,min.freq=10,random.color=T,color=color,use.r.layout=F)



## texter

ms.t <- ddply(ms[ms$type=="texter",],.(conv_id),pastems)
conv_id <- ms.t[[1]]
View(ms.t)
ms.t <- VCorpus(DataframeSource(ms.t))

#print(ms.c)
#inspect(ms.c[1:2])

ms.t <- tm_map(ms.t,stripWhitespace)
ms.t <- tm_map(ms.t,content_transformer(tolower))
ms.t <- tm_map(ms.t,removeWords,stopwords("english"))
ms.t <- tm_map(ms.t,stemDocument)
tdm.t <- TermDocumentMatrix(ms.t)
tdm.t.r <- removeSparseTerms(tdm.t,0.8)


# what the texter is saying
m.t <- as.matrix(tdm.t.r)
v.t <- sort(rowSums(m.t),decreasing=T)
words.t <- names(v.t)
d.t <- data.frame(word=words.t,freq=v.t)

display.brewer.all()
color <- brewer.pal(12,"Set3")

wordcloud(d.t$word,d.t$freq,min.freq=10,random.color=T,color=color,use.r.layout=F)