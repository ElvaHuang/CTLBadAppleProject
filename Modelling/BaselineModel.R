

##--  baseline model with quantitative variables  --##

## This script should be executed after exploratory

rm(list=ls())
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

load("usefulVariable_c.Rda")
load("valiIndex.Rda")



## variables to use in the baseline model
# conv_length_ms
# I
# qsratio
# excla


# variable for training
validation <- useful1[vali.index,]
train <- useful1[-vali.index,]

## run a linear regression
ols <- lm(t_rating.n ~ conv_length_ms + I + qsratio + excla + robo + text_freq + ms_length_char,data=train)
summary(ols)

# prediction





## change rating to 0-1 scale and use logistic regression

useful.binary <- useful1[useful1$t_rating.n %in% c(-1,1),]

ols.binary <- lm(t_rating.n ~ conv_length_ms + I + qsratio + excla + robo + text_freq + ms_length_char,data=useful.binary)
summary(ols.binary)

useful.bad <- useful1
useful.bad$t_rating.n[useful.bad$t_rating.n %in% c(0,1)] <- 0
useful.bad$t_rating.n[useful.bad$t_rating.n==-1] <- 1

logit <- glm(t_rating.n ~ conv_length_ms + I + qsratio + excla, data = useful.bad, family = "binomial")
summary(logit)



