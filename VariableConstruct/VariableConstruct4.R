
##--  Variable Construction VI Regular Expression--##

## database=ms_level, difficulty=2,priority=1  ##

## This script should be executed after VariableConstruct4 and before VariableConstruct6

## variable list
# question/statement ratio
# robotic message/statement ratio
# times of using the texter name
# number of exclamation mark
# length of specilist first msg
# if specialist shares name or not

##  ----------------------------------------------------------------------------------------  ##

rm(list=ls())

##set working directory
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")
load("ms_level.Rda")

## preprocessing
names(ms)
str(ms)

for(i in c(1:3,6)){
  ms[,i] <- as.factor(ms[,i])
}

m_time <- strptime(ms$m_time, "%Y-%m-%d %H:%M:%S",tz="UTC")
any(is.na(m_time)) # no missing value 
m_time <- as.POSIXct(m_time)  
ms$m_time <- m_time

# clean out invalid regular expression
require(stringr)
ms$message <- str_replace_all(ms$message,"\\*","")
ms$message <- str_replace_all(ms$message,"\\("," ")
ms$message <- str_replace_all(ms$message,"\\)"," ")
ms$message <- str_replace_all(ms$message,"\\\\"," ")
ms$message <- str_replace_all(ms$message,"\\["," ")
ms$message <- str_replace_all(ms$message,"\\]"," ")
ms$message <- str_replace_all(ms$message,"\\+"," ")
ms$message <- str_replace_all(ms$message,"\\^"," ")
ms$message <- str_replace_all(ms$message,"\\{"," ")
ms$message <- str_replace_all(ms$message,"\\}"," ")
ms$message <- str_replace_all(ms$message,"\\.\\.\\.","")




## variable construction using regular expression

## question to statement ratio

# function
qsratio <- function(data){
  data <- as.matrix(data$message)
  sum(str_detect(data,"\\?"))/length(data)
}

# test
# test <- ms[ms$conv_id==levels(ms$conv_id)[1],]
# qsratio(test)
# qsratio.type(test)

# apply
require(plyr)

qsratio_result <- ddply(ms[ms$type=="counselor",],.(conv_id),qsratio)
names(qsratio_result)[2]<- "qsratio"
  ## one line can have several questions

## deep dive on type specific question to total question ratio
open <- c("what","How","When","Where")
qqratio.type <- function(data){
  data <- as.matrix(data$message)
  index <- which(str_detect(data,"\\?"))
  
  why <- str_detect(data[index],ignore.case("why"))
  why.number <- sum(why)
  whyqq <- why.number/length(index)
  index <- index[!why]
  
  if (length(index)==0){
    open.number <- 0
  }else if(length(index)==1){
    open.number <- sum(sapply(ignore.case(open),grepl,data[index]))
  }else{
    open.number <- rowSums(sapply(ignore.case(open),grepl,data[index]))
  }
  type <- ifelse(open.number>=1,1,0)
  openqq <- sum(type)/length(index)
  closeqq <- (length(type)-sum(type))/length(index)
  cbind(whyqq,openqq,closeqq)
}
  
qqratio.type_result <- ddply(ms[ms$type=="counselor",],.(conv_id),qqratio.type)
  


## deep dive on question to statement ratio
open <- c("what","How","When","Where")
qsratio.type <- function(data){
  data <- as.matrix(data$message)
  index <- which(str_detect(data,"\\?"))
  
  why <- str_detect(data[index],ignore.case("why"))
  why.number <- sum(why)
  whyqs <- why.number/length(data)
  index <- index[!why]
  
  if (length(index)==0){
    open.number <- 0
  }else if(length(index)==1){
    open.number <- sum(sapply(ignore.case(open),grepl,data[index]))
  }else{
    open.number <- rowSums(sapply(ignore.case(open),grepl,data[index]))
  }
  type <- ifelse(open.number>=1,1,0)
  openqs <- sum(type)/length(data)
  closeqs <- (length(type)-sum(type))/length(data)
  cbind(whyqs,openqs,closeqs)
}

qsratio.type_result <- ddply(ms[ms$type=="counselor",],.(conv_id),qsratio.type)


## robotic message number

# function
canned <- c("Hi, my name is [[:alpha:]]+. Would you mind sharing your name?","I couldn't understand your last msg.  tech issue on my end.) Can you send it again in two parts?  Short msgs help.)",
            "I haven't heard from you in a little while. If you respond soon, I will be happy to continue talking.",
            "It seems you've stepped away from your phone. Please contact us again when you have more time to text.",
            "I'm glad you texted to find out what we're all about! The specialists at CTL are here for you 24/7. Text us again if you need support.",
            "I will need to terminate this session if you continue to use the Text Line inappropriately.",
            "I need to end this conversation due to inappropriate use of the Text Line, goodbye.",
            "Since we are a crisis text program I want to ask you, are you having any thoughts of suicide today?",
            "Crisis Text Line is a 24/7 support line for young people.   You can text us about anything that's on your mind.",
            "I'm sorry- she isn't available, but I'm here to listen")

robo <- function(data){
  data <- as.matrix(data[,"message"])
  sum(sapply(canned,grepl,data))   #difference between str_detect and grepl
}

# apply
robo_result <- ddply(ms[ms$type=="counselor",],.(conv_id),robo)
names(robo_result)[2] <- "robo"


## number of exclamation mark counselor typed 

excla <- function(data){
  data <- as.matrix(data[,"message"])
  sum(str_detect(data,"!")) 
}

excla_result <- ddply(ms[ms$type=="counselor",],.(conv_id),excla)
names(excla_result)[2] <- "excla.c"
# validate
#View(ms[ms$conv_id==excla_result[which.max(excla_result[,2]),1], ])


## exclamation mark matching ratio
excla_result.t <- ddply(ms[ms$type=="texter",],.(conv_id),excla)
names(excla_result.t)[2] <- "excla.t"
excla.ct <- merge(excla_result,excla_result.t,by.x="conv_id",by.y="conv_id")

matchratio <- function(data){
  if (data[1]==0 & data[2]==0){
    ratio <- 1
  }else if(data[1]==0 | data[2]==0){
    ratio <- 0
  }else{
    ratio <- min(data[1:2])/max(data[1:2])
  }
  return(ratio)
}

# validate
# matchratio(excla.ct[100:101,])

# apply
exclaratio <- apply(excla.ct[,2:3],1,matchratio)
exclaratio <- data.frame(conv_id=excla.ct$conv_id,exclaratio=exclaratio)



## times of using the word "I" or "I'm"

I <- function(data){
  data <- as.matrix(data[,"message"])
  sum(str_detect(data,ignore.case("I ")) | str_detect(data,ignore.case("I'm"))) 
}

I_result <- ddply(ms[ms$type=="counselor",],.(conv_id),I)
names(I_result)[2] <- "I"

# validate
#View(ms[ms$conv_id==I_result[which.max(I_result[,2]),1], ])


textVar <- merge(I_result,excla_result,by.x="conv_id",by.y="conv_id")
textVar <- merge(exclaratio,textVar,by.x="conv_id",by.y="conv_id")
textVar <- merge(qsratio_result,textVar,by.x="conv_id",by.y="conv_id")
textVar <- merge(qsratio.type_result,textVar,by.x="conv_id",by.y="conv_id")
textVar <- merge(qqratio.type_result,textVar,by.x="conv_id",by.y="conv_id")
textVar <- merge(robo_result,textVar,by.x="conv_id",by.y="conv_id")

save(textVar,file="usefulVariable2.Rda")



## times of using the texter name

# solution 1 - tagged
# solution 2 - dictionary of names
# solutio 3 - dictionary of regular words

