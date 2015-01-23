

##--  Varialbe Construction II  --##

## database=message, difficulty=1, priority=1  ##

## This script should be executed after VariableConstruct1 and before VariableConstruct3

## variable list (conversation level)##

# total number of messages per conversation
# average character length of a counselor response
# average response time for a counselor per conversation 


##  ----------------------------------------------------------------------------------------  ##
rm(list=ls())

##set working directory
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

##read in data
load("ms_level.Rda")
names(ms)

## convert the time variable

## message time
head(ms$m_time)
m_time <- strptime(ms$m_time, "%Y-%m-%d %H:%M:%S",tz="UTC")
any(is.na(m_time)) # no missing value 

m_time <- as.POSIXct(m_time)   # this stored dates as seconds from January 1, 1970 & POSIXlt store them as list

ms$m_time <- m_time

## variable construction

require(plyr)

## number of all messages per conv
ms$conv_id <- as.factor(ms$conv_id)
length(unique(ms$conv_id))

# ?ddply  like by, ddply can only have anonymous functions, but it returns a data frame
conv_length_ms <- ddply(ms,.(conv_id),nrow)
colnames(conv_length_ms) <- c("conv_id","conv_length_ms")
#View(conv_length_ms)


## average length of counselor's message

count_message_length <- function(data){
  return(mean(nchar(data[data$type=="counselor",]$message),na.rm=T))
}

ms_length_char <- ddply(ms,.(conv_id),count_message_length)
colnames(ms_length_char) <- c("conv_id","ms_length_char")
#View(ms_length_char)     # why is NaN


## write out data
construct <- merge(x=conv_length_ms,y=ms_length_char,by.conv_length_ms="conv_id",by.ms_length_char="conv_id")

save(construct,file="ms_level_construct2.Rda")
save(ms,file="ms_level.Rda")



## -- additional script for counting response time

## average response time of counselor after the texter texts for only engaged conversations

if(F){
  count_response <- function(data){
    data <- data[order(data$m_time), ]
    index.c <- which(data$type=="counselor") 
    #print(index.c)
    c.s <- index.c[1]
    #print(c.s)
    c.e <- index.c[length(index.c)]
    #print(c.e)
    
    index.t <- which(data$type=="texter")
    #print(index.t)
    if(!any(index.t > c.s)){
      return("Not Engaged")
    }else{
      start <- min(which(index.t > c.s))
      #print(start)
      end <- max(which(index.t < c.e))
      #print(end)
      range <- index.t[start:end]
      #print(range)
      
      minute = 0
      count = 0
      for (i in range){
        for (j in i+1:range[length(range)]){
          if(data$type[j]=="counselor"){
            minute = minute + as.numeric(data$m_time[j]-data$m_time[i])
            #print(minute)
            count=count+1
            #print(count)
          }
          break
        }   
      }
      return(minute/count)
    }
  }
  
  result <- ddply(ms,.(conv_id),count_response)
}
