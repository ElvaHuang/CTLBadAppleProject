
##--  Varialbe Construction I  --##
## database=convo, difficulty=1, critical=1  ##

## This script should be executed after DataPreprocessing and before VariableConstruct2


## variable list (conversation level)##

# wait time in a queue
# severity of issues
# length of convo
# specialist rating
# texter rating
# ave concurrent conversations (no)


##  ----------------------------------------------------------------------------------------  ##


rm(list=ls())

##set working directory
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

##read in data

#load("conv_level_random.Rda")
load("conv_level.Rda")



## teen initial response time

##teen text in
head(conv$conv_start)
conv_start <- strptime(conv$conv_start, "%Y-%m-%d %H:%M:%S",tz="UTC")
#any(is.na(conv_start))
#View(conv[which(is.na(conv_start)),])
conv_start <- as.POSIXct(conv_start)   # this stored dates as seconds from January 1, 1970 & POSIXlt store them as list

##teen finishes system questions
head(conv$queue_enter)
queue_enter <- strptime(conv$queue_enter, "%Y-%m-%d %H:%M:%S",tz="UTC")
queue_enter <- as.POSIXct(queue_enter)   # this stored dates as seconds from January 1, 1970 & POSIXlt store them as list

##compute initial response time   (sec)
texter_response <- queue_enter - conv_start
texter_response_s <- as.numeric(texter_response)

#append texter_response to conv data frame
conv$texter_response <- texter_response_s
conv$queue_enter2 <- queue_enter
conv$conv_start2 <- conv_start



##convo length

##real start of conversation
head(conv$queue_exit)
queue_exit <- strptime(conv$queue_exit, "%Y-%m-%d %H:%M:%S",tz="UTC")
queue_exit <- as.POSIXct(queue_exit)   # this stored dates as seconds from January 1, 1970 & POSIXlt store them as list

##end of conversation
conv_end <- strptime(conv$conv_end, "%Y-%m-%e %H:%M:%S",tz="UTC")
conv_end <- as.POSIXct(conv_end)   # this stored dates as seconds from January 1, 1970 & POSIXlt store them as list

##compute conv length secs
conv_length <- conv_end-queue_exit
conv_length_s <- as.numeric(conv_length)

#append conv_length, and queue_exit,conv_end to conv data frame
conv$conv_length <- conv_length_s
conv$queue_exit2 <- queue_exit
conv$conv_end2 <- conv_end

  ## remove conversations on daylight saving hours 24
  #conv <- conv[!is.na(conv$conv_length),]
  ## remove records of which the conversation length exceeds 8 hours
  #summary(conv$conv_length)
  #require(ggplot2)
  #ggplot(data=conv[conv$conv_length<=8*60*60,]) + geom_density(aes(x=conv_length))
  conv <- conv[conv$conv_length<=8*60*60,]



##wait time in a queue

##enter queue
#queue_enter already constructed

##exit queue
#queue_exit already constructed

##compute waiting time sec
wait_queue <- conv$queue_exit2-conv$queue_enter2    #2 missing value
wait_queue_s <- as.numeric(wait_queue)

#append wait_queue_m to conv data frame
conv$wait_queue <- wait_queue_s

  ##remove records where wait time is NA
  #conv <- conv[!is.na(conv$wait_queue),]



##specialist rating
unique(conv$Q36_visitor_feeling)  #much better is an old version, need to exclude that
sum(conv$Q36_visitor_feeling=="Much better",na.rm=T)
conv$Q36_visitor_feeling[conv$Q36_visitor_feeling=="Much better"] <- "reduced"

specialist_rating<- as.factor(conv$Q36_visitor_feeling) 
levels(specialist_rating)

sum(is.na(specialist_rating))   #32706 missing value

#append specialist_rating to conv data frame
conv$specialist_rating <- specialist_rating



##texter rating
unique(conv$conv_rating)

conv$conv_rating[conv$conv_rating=="-1"] <- "worse"
conv$conv_rating[conv$conv_rating=="1"] <- "same"
conv$conv_rating[conv$conv_rating=="2"] <- "better"

texter_rating<- as.factor(conv$conv_rating) 

#texter_rating<- as.factor(conv_sample$conv_rating) 

levels(texter_rating)

sum(is.na(texter_rating))   

#append specialist_rating to conv data frame
conv$texter_rating <- texter_rating



##severity of issues

severity <- numeric(length=nrow(conv))

#require(stringr)
#sapply(1:length(severity),function(i){
#  if (grepl("yes",conv$Q75_active_rescue[i]) |
#        grepl("rescue",conv$Q8_conv_resolution[i])){
#    severity[i]==5
#  }else if(grepl("in_progress",conv$Q66_suicidal_intent[i])){
#    severity[i]==4
#  }else if(grepl("suicide_plan",conv$Q66_suicidal_intent[i]) |
#             grepl("suicide_timeframe",conv$Q66_suicidal_intent[i]) |
#             grepl("suicide_prep",conv$Q66_suicidal_intent[i]) |
#             grepl("suicide_expressed_intent",conv$Q66_suicidal_intent[i]) |
#             !is.na(conv$Q67_suicidal_capability[i])){
#    severity[i]==3
#  }else if(!is.na(conv$Q66_suicidal_intent[i])){
#    severity[i]==2
#  }else if(grepl("suicide",conv$Q13_issues[i])){
#    severity[i]==1
#  }else{
#    severity[i]==NA
#  }
#})


for (i in 1:length(severity)){
  if (grepl("yes",conv$Q75_active_rescue[i]) |
        grepl("rescue",conv$Q8_conv_resolution[i])){
    severity[i]<-5
  }else if(grepl("in_progress",conv$Q66_suicidal_intent[i])){
    severity[i]<-4
  }else if(grepl("suicide_plan",conv$Q66_suicidal_intent[i]) |
             grepl("suicide_timeframe",conv$Q66_suicidal_intent[i]) |
             grepl("suicide_prep",conv$Q66_suicidal_intent[i]) |
             grepl("suicide_expressed_intent",conv$Q66_suicidal_intent[i]) |
             !is.na(conv$Q67_suicidal_capability[i])){
    severity[i]<-3
  }else if(!is.na(conv$Q66_suicidal_intent[i])){
    severity[i]<-2
  }else if(grepl("suicide",conv$Q13_issues[i])){
    severity[i]<-1
  }else{
    severity[i]<-NA
  }
}

unique(severity)

##append severity to conv data frame
conv$severity <- severity


## save the temporary result
save(conv,file="conv_level_construct1.Rda")



