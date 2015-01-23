##-- validate correlation  --##

## This script should be executed after exploratory and before Baseline Model

## variables and exploratory results

# strong correlation
# conv_length    num sec
# conv_length_ms  num
# qsratio
# whyqs
# openqs
# closeqs
# I

# weak correlation
# ms_length_char  num char
# text_freq     num sec
# excla      num
# severity   categorical 1-5

# no correlation
# wait_queue num sec
# aveRes     num sec
# robo       num
# texter_response num sec

# outcome variable
# texter_rating categorical

# partial correlation

rm(list=ls(all=T))
gc()
load("usefulVariable_c.Rda")
names(useful)
str(useful)

## take a random sample of the usefulVariable_c.Rda
set.seed(12345)
random.index <- sample(1:10921,size=5000,replace=F)
useful.random <- useful[random.index,]
rm(useful)

## simple randomization check
require(ggplot2)
require(gridExtra)

g1 <- ggplot(data=useful) + geom_histogram(aes(x=texter_rating,fill=texter_rating))
g2 <- ggplot(data=useful.random) + geom_histogram(aes(x=texter_rating,fill=texter_rating))
grid.arrange(g1,g2,ncol=1)


## construct an ordered texter_rating variable t_rating.n with value -1,0,1
#useful$t_rating.n <- 0
#useful$t_rating.n[useful$texter_rating=="better"] <- 1
#useful$t_rating.n[useful$texter_rating=="worse"] <- -1
#table(useful$t_rating.n)
#save(useful,file="usefulVariable_c.Rda")


## compute distance correlation
require(energy)

#dependent <- names(useful)[c(2:6,12:13,16:20)]
#corVali <- function(names){
#  cor <- cor(useful$names,useful$t_rating.n)
#  dcor <- dcor(useful$names,useful$t_rating.n)
#  data.frame(cor=cor, dcor=dcor)
#}
#sapply(dependent,corVali)

#sapply(dependent,function(x)is.numeric(useful$x))

useful.r <- useful[!(is.na(useful$severity) | is.na(useful$aveRes)),]

unique(useful.r$severity)
useful.r$severity.b[useful.r$severity %in% 1:4] <- 0
useful.r$severity.b[useful.r$severity==5] <- 1


## close ended question
cor(useful$closeqs,useful$t_rating.n)  # -0.1116271
dcor(useful$closeqs,useful$t_rating.n)    # 0.1143247
closeqs <- dist(useful.random$closeqs)
rating <- dist(useful.random$t_rating.n)
test.closeqs <- dcor.ttest(closeqs,rating,distance=T)
test.closeqs$p.value   # 0

## open ended question
cor(useful$openqs,useful$t_rating.n)  # -0.03309146
dcor(useful$openqs,useful$t_rating.n)    # 0.06220551
openqs <- dist(useful.random$openqs)
rating <- dist(useful.random$t_rating.n)
test.openqs <- dcor.ttest(openqs,rating,distance=T)
test.openqs$p.value   # 0

## why question
cor(useful$whyqs,useful$t_rating.n)  # -0.008151685
dcor(useful$whyqs,useful$t_rating.n)    # 0.03476488
whyqs <- dist(useful.random$whyqs)
rating <- dist(useful.random$t_rating.n)
test.whyqs <- dcor.ttest(whyqs,rating,distance=T)
test.whyqs$p.value   # 0.00412026 

## strong correlation
cor(useful$qsratio,useful$t_rating.n)  # -0.1068261
dcor(useful$qsratio,useful$t_rating.n)    # 0.1115603
qsratio <- dist(useful$qsratio)
rating <- dist(useful$t_rating.n)
test.qs <- dcor.ttest(qsratio,rating,distance=T)
test.qs$p.value   # 0

cor(useful$I,useful$t_rating.n)    # 0.1035112
dcor(useful$I,useful$t_rating.n)    # 0.111693
I <- dist(useful.random$I)
rating <- dist(useful.random$t_rating.n)
test.I <- dcor.ttest(I,rating,distance=T)
test.I$p.value  # 0

cor(useful$conv_length,useful$t_rating.n)    # 0.1066394
dcor(useful$conv_length,useful$t_rating.n)    # 0.1153874
conv_length <- dist(useful.random$conv_length)
test.conv_length <- dcor.ttest(conv_length,rating,distance=T)
test.conv_length$p.value  # 0

cor(useful$conv_length_ms,useful$t_rating.n)    # 0.154137
dcor(useful$conv_length_ms,useful$t_rating.n)    # 0.1622817
conv_length_ms <- dist(useful.random$conv_length_ms)
test.conv_length_ms <- dcor.ttest(conv_length_ms,rating,distance=T)
test.conv_length_ms$p.value   # 0

cor(useful$excla,useful$t_rating.n)  # 0.1276266
dcor(useful$excla,useful$t_rating.n)  # 0.1421299
excla <- dist(useful.random$excla)
test.excla <- dcor.ttest(excla,rating,distance=T)
test.excla$p.value   # 0


## weak correlation
cor(useful$ms_length_char,useful$t_rating.n) # 0.07270943
dcor(useful$ms_length_char,useful$t_rating.n)  # 0.06556077
ms_length_char <- dist(useful.random$ms_length_char)
test.ms_length_char <- dcor.ttest(ms_length_char,rating,distance=T)
test.ms_length_char$p.value   # 0

cor(useful$text_freq,useful$t_rating.n)  # -0.01843708
dcor(useful$text_freq,useful$t_rating.n)  # 0.06556077
text_freq <- dist(useful.random$text_freq)
test.text_freq <- dcor.ttest(text_freq,rating,distance=T)
test.text_freq$p.value   # 1.130873 e-12

cor(useful$robo,useful$t_rating.n)    # -0.09467192
dcor(useful$robo,useful$t_rating.n)    # 0.08015079
robo <- dist(useful.random$robo)
test.robo <- dcor.ttest(robo,rating,distance=T)
test.robo$p.value   # 0



## no correlation
cor(useful$wait_queue,useful$t_rating.n)    # 0.007356707
dcor(useful$wait_queue,useful$t_rating.n)    # 0.02335009
wait_queue <- dist(useful.random$wait_queue)
test.wait_queue <- dcor.ttest(wait_queue,rating,distance=T)
test.wait_queue$p.value   # 0.2692998

cor(useful.r$aveRes,useful.r$t_rating.n)    # 0.00290877
dcor(useful.r$aveRes,useful.r$t_rating.n)    # 0.03000252
aveRes <- dist(useful.r$aveRes)
rating <- dist(useful.r$t_rating.n)
test.aveRes <- dcor.ttest(aveRes,rating,distance=T)
test.aveRes$p.value   # 0.5256162 

cor(useful$texter_response,useful$t_rating.n)    # -0.00460685
dcor(useful$texter_response,useful$t_rating.n)    # 0.01031075
texter_response <- dist(useful.random$texter_response)
rating <- dist(useful.random$t_rating.n)
test.texter_response <- dcor.ttest(texter_response,rating,distance=T)
test.texter_response$p.value   # 1.763604e-05

cor(useful.r$severity,useful.r$t_rating.n)  # -0.03057622
dcor(useful.r$severity,useful.r$t_rating.n)  # 0.02600059
severity <- dist(useful.r$severity)
rating <- dist(useful.r$t_rating.n)
test.severity <- dcor.ttest(severity,rating,distance=T)
test.severity$p.value   # 0.4219647 

cor(useful.r$severity.b,useful.r$t_rating.n)  # -0.04644955
dcor(useful.r$severity.b,useful.r$t_rating.n)  # 0.04513739
severity.b <- dist(useful.r$severity.b)
test.severity.b <- dcor.ttest(severity.b,rating,distance=T)
test.severity.b$p.value   # 0.03044892                                   # this can be used as a control


## to see if predictors are correlated

cor(useful$conv_length,useful$conv_length_ms) # 0.6695173
dcor(useful$conv_length,useful$conv_length_ms) # 0.6623189



## test partial correlation - ppcor

install.packages("ppcor")
require(ppcor)

pcor.useful <- pcor(useful.r[,c("t_rating.n","aveRes","wait_queue","texter_response","conv_length_ms","ms_length_char","text_freq","robo","whyqs","openqs","closeqs","I","excla","severity.b")])
pcor.useful$estimate
View(pcor.useful$p.value)


