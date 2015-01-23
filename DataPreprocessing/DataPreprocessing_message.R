####      Data Preprocessing     ####

rm(list=ls())

##set working directory
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

##read in data
ms <- read.csv("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple/elva_message_level.csv",header=F,sep=",",na.string="",colClass="character")   
colnames(ms) <- c("m_id","conv_id","actor_id","message","type","area_code","m_time")

load("conv_level_random.Rda")
load("conv_level.Rda")

##clean missing values or redundant records after read in 
str(ms)

##check missing data
sapply(names(ms),function(x)sum(is.na(ms[,x])))
   ## all type missing
   ## 285 messages missing



##fix the actor type variable
unique(ms$area_code)
ms$type <- "texter"
ms$type[ms$area_code=="NA" & ms$actor_id!="1"] <- "counselor"
ms$type[ms$actor_id=="1"] <- "system"
unique(ms$type)
ms$type <- as.factor(ms$type)

## match message level data with convo level
ms <- ms[ms$conv_id %in% conv$conv_id,]

## take a random sample of conversations
ms_sample <- ms[ms$conv_id %in% conv_sample$conv_id,]

##check missing data in the sample
sapply(names(ms),function(x)sum(is.na(ms[,x])))
sapply(names(ms_sample),function(x)sum(is.na(ms_sample[,x])))

  ## 5 message missing
  ##View(ms[is.na(ms$message),])
  ## delete the record
  #ms <- ms[!is.na(ms$message),]
  #ms_sample <- ms_sample[!is.na(ms_sample$message),]


save(ms_sample,file="ms_level_random.Rda")  
save(ms,file="ms_level.Rda")
