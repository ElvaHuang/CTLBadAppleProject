####      Data Preprocessing     ####

rm(list=ls())

##set working directory
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

##read in data
conv <- read.csv("elva_conversation_level.csv",header=T,sep=",",na.string="",colClass="character")   

##clean missing values or redundant records after read in 
#str(conv)
#names(conv)

sum(is.na(conv$conv_id))  # no missing value
#conv <- conv[-which(is.na(conv$conv_id)),]  #now it matches the # of records in excel

##check missing data
sapply(names(conv),function(x)sum(is.na(conv[,x])))
  #no missing data in conv_id, can use the variable as our key
  #queue_exit has 247 missing value, conv_end has 17 missing value
#View(conv[which(is.na(conv$conv_end)),]) 
#View(conv[which(is.na(conv$queue_exit)),])    # because of ongoing conversation or still in the queue

##remove records without queue_exit or conv_end

conv <- conv[!is.na(conv$queue_exit),]
conv <- conv[!is.na(conv$conv_end),]

##remove records where Crisis Center=Crisis Text Line

unique(conv$crisis_center)
conv <- conv[!(conv$crisis_center=="Crisis Text Line"),]

save(conv,file="conv_level.Rda")


