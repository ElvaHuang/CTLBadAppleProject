
##--  Train Validate Split  --##

rm(list=ls(all=T))
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

load("conv_level_complete.Rda")

set.seed(12345)
vali.index <- sample(1:nrow(conv_complete),2500,replace=F)
save(vali.index,file="valiIndex.Rda")
conv_validation <- conv_complete[vali.index,]
save(conv_validation,file="conv_validation.Rda")

conv <- conv_complete[-vali.index,] # this is for training

save(conv,file="conv_train.Rda")
