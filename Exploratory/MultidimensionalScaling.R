
##--  Multidimensional Scaling  --##

rm(list=ls(all=T))
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

load("ms_level.Rda")
load("conv_level_construct1.Rda")


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
rm(ms)
conv_merge <- data.frame(conv_id=conv$conv_id,texter_rating=conv$texter_rating)
rm(conv)
ms.c_rated <- merge(conv_merge,ms.c,by.x="conv_id",by.y="conv_id")
rm(ms.c)
rm(conv_merge)

ms.c <- ms.c_rated[,3]



## Playing with the tm package 
require(tm)

## counselor 
ms.c <- VCorpus(DataframeSource(ms.c_rated))
ms.c <- tm_map(ms.c,stripWhitespace)
ms.c <- tm_map(ms.c,content_transformer(tolower))
ms.c <- tm_map(ms.c,removeWords,stopwords("english"))
ms.c <- tm_map(ms.c,stemDocument)

dtm.c <- DocumentTermMatrix(ms.c)  # for ordination
dtm.c.r <- removeSparseTerms(dtm.c,0.8)



## mds

require(stats)
dist.c <- dist(dtm.c.r,method="euclidean",p=2)

cmd.c <- cmdscale(dist.c,eig=T)
cmd.c.df <- as.data.frame(cbind(cmd.c$points,ms.c_rated$texter_rating))
names(cmd.c.df)

require(ggplot2)
ggplot(data=cmd.c.df, aes(x=V1,y=V2)) + geom_point(aes(color=texter_rating)) +
  ggtitle("Counselor")






## texter
ms.t <- VCorpus(DataframeSource(ms.t))
#print(ms.c)
#inspect(ms.c[1:2])

ms.t <- tm_map(ms.t,stripWhitespace)
ms.t <- tm_map(ms.t,content_transformer(tolower))
ms.t <- tm_map(ms.t,removeWords,stopwords("english"))
ms.t <- tm_map(ms.t,stemDocument)
dtm.t <- DocumentTermMatrix(ms.t)
dtm.t.r <- removeSparseTerms(dtm.t,0.8)

## apply to only texter message
ms.t <- ddply(ms[ms$type=="texter",],.(conv_id),pastems)
conv_id <- ms.t[[1]]
View(ms.t)


## pca 
# install.packages("kernlab")
  # this has a memory problem it seems
require(kernlab)

k <- kpca(m.c,features = 2)
plot(rotated(k),col)







## for texter
dist.t <- dist(dtm.t,method="euclidean",p=2)


cmd.t <- cmdscale(dist.t,eig=T)
cmd.t.df <- as.data.frame(cbind(cmd.t$points,conv_id))
texter_rating <- conv_sample$texter_rating[match(conv_sample$conv_id,conv_id)]
cmd.t.df <- cbind(cmd.t.df,texter_rating)

names(cmd.t.df)
require(ggplot2)
ggplot(data=cmd.t.df, aes(x=V1,y=V2)) + geom_point(aes(color=texter_rating))

