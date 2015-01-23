
##--  Varialbe Construction III Merge --##

## database=constracted, difficulty=1,priority=1  ##

## This script should be executed after VariableConstruct2 and before VariableConstruct3

##  ----------------------------------------------------------------------------------------  ##

rm(list=ls())

##set working directory
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")
load("conv_level_construct1.Rda")
load("ms_level_construct2.Rda")
load("ms_level.Rda")

names(conv)



## simplify and merge database
useful <- conv[,c("conv_id","texter_id","counselor_id","crisis_center",
                         "specialist_name","engaged","conv_length","wait_queue",
                         "specialist_rating","texter_rating","severity","texter_response")]

useful <- merge(x=useful,y=construct,by.useful="conv_id",by.construct="conv_id")
str(useful)



## frequency of text 

freq <- useful$conv_length/useful$conv_length_ms
useful$text_freq <- freq




## ave response time after teen text 


## test ---- 
#data <- ms[ms$conv_id=="1001",]
#count_response(data)

#test <- function(data){
#  data <- ms[order(ms$m_time),]
#  index.c <- which(data$type=="counselor")
#  c.s <- index.c[1]
#  c.e <- index.c[length(index.c)]
  
#  index.t <- which(data$type=="texter")
#  condition <- (index.t > c.s)
#  return(condition)
#}

#result <- ddply(ms,.(conv_id),test)


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
colnames(result) <- c("conv_id","aveRes")

## merge the result with useful 
useful <- merge(x=result,y=useful,by.result="conv_id",by.useful="conv_id")
str(useful)
save(useful,file="usefulVariable.Rda")




## calculate concurrent conversations 
## still not working

if(F){
  x <- unique(conv$counselor_id)
  length(x)
  
  data1=conv[conv$counselor_id=="36",]
  
  cal_concurrent(data1)
  cal_concurrent <- function(data){
    result <- numeric()
    for (i in 1:(nrow(data)-1)){
      concurrent=1
      for (j in (i+1):(nrow(data))){
        if ( (data$queue_exit2[j]<data$conv_end2[i]) & (data$conv_end2[j] > data$queue_exit2[i]) ){
          concurrent=concurrent+1
          data <- data[-j,]
        }
      }
      result <- c(result,concurrent)
    }
    ave <- sum(result)/length(result)
    return(ave)
  }
  cal_concurrent(conv)
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## test
  for(x in levels(conv$counselor_id)[1:5]){
    data1 = conv[conv$counselor_id==x,]
    cal_concurrent(data1)
  }
  
  counselor_concurrent <- ddply(conv,.(counselor_id),cal_concurrent)
  
  any(is.na(conv$queue_exit2))
  any(is.na(conv$conv_end2))
}








