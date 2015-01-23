
##--    Exploratory Analysis  --##

## This script should be executed after VariableConstruct3 and before CorValidation

## variable to test correlation

# conv_length    num sec
# conv_length_ms  num
# qsratio
# I
# ms_length_char  num char
# text_freq     num sec
# excla      num
# severity   categorical 1-5
# wait_queue num sec
# aveRes     num sec
# robo       num
# texter_response num sec

# texter_rating categorical



rm(list=ls())
setwd("C:/Users/Rongyao/SkyDrive (2)/CTL/BadApple")

## read in data 
load("usefulVariable.Rda")
load("usefulVariable2.Rda")
useful <- merge(useful,textVar,by.x="conv_id",by.y="conv_id")
str(useful)



## convert variables

useful$texter_id <- as.factor(useful$texter_id)
useful$counselor_id <- as.factor(useful$counselor_id)
useful$crisis_center <- as.factor(useful$crisis_center)
useful$specialist_name <- as.factor(useful$specialist_name)
useful$engaged <- as.factor(useful$engaged)

str(useful)

## check missing ratings

sum(is.na(useful$texter_rating))
table(useful$texter_rating)


## take equal size sample

better.index <- which(useful$texter_rating=="better")
set.seed(123)
select.index.b <- sample(better.index,982,replace=F)
same.index <- which(useful$texter_rating=="same")
set.seed(123)
select.index.s <- sample(same.index,982,replace=F)
worse.index <- which(useful$texter_rating=="worse")

sample.index <- c(select.index.b,select.index.s,worse.index)
useful.sample <- useful[sample.index,]

## correlation, distance correlation, visualization, linear regression

require(ggplot2)
require(ggthemes)
require(scales)
require(gridExtra)



## conversation length (mins) and texter_rating

cols <- c("better"="#93C91C","same"="#80827D","worse"="#F51857")
g1 <- ggplot(data=useful,aes(x=texter_rating,y=conv_length/60)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.3,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Box Plot") + ylab("Conversation Length (Mins)") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  scale_color_manual(name="Texter Rating",values=cols) +
  theme(legend.position="none") +
  scale_y_continuous(breaks=pretty_breaks(n=10))


g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=conv_length/60,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=conv_length/60,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=conv_length/60,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Conversation Length (Mins)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Conversation Length and Texter Rating",gp=gpar(fontsize=20)))

if(F){
  useful.order <- useful[order(useful$conv_length,decreasing=FALSE),]
  useful.order$order <- 1:nrow(useful.order)
  
  ggplot() + geom_bar(data=useful.order,aes(x=order,y=conv_length/60,fill=texter_rating),stat="identity",alpha=0.4) +
    ggtitle("Conversation Length and Texter Rating") + ylab("") + xlab("") + 
    theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), panel.grid.minor=element_blank())
}



## conversation length in terms of messages sent

summary(useful$conv_length_ms)

g1 <- ggplot(data=useful,aes(x=texter_rating,y=conv_length_ms)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0)+
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))


g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=conv_length_ms,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=conv_length_ms,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=conv_length_ms,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Conversation Length (messages)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Conversation Length (Total Messages) and Texter Rating",gp=gpar(fontsize=20)))




## wait_queue and texter rating

g1 <- ggplot(data=useful,aes(x=texter_rating,y=log(wait_queue))) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("Log Wait Time in a Queue") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  scale_color_manual(name="Texter Rating",values=cols) +
  theme(legend.position="none") +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=log(wait_queue),fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=log(wait_queue),fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=log(wait_queue),fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Log Wait Time in Queue") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Wait Time in Queue and Texter Rating",gp=gpar(fontsize=20)))
        

## average message character length

g1 <- ggplot(data=useful,aes(x=texter_rating,y=ms_length_char)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=ms_length_char,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=ms_length_char,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=ms_length_char,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Average Number of Characters Per Message Sent by Counselors") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Average Message Length and Texter Rating",gp=gpar(fontsize=20)))



## text_freq convo length/# messages

g1 <- ggplot(data=useful,aes(x=texter_rating,y=text_freq)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("Message Frequency(Secs)") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="same",],aes(x=text_freq,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=text_freq,fill="worse"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="better",],aes(x=text_freq,fill="better"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Message Frequency(Secs)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Message Exchange Frequency and Texter Rating",gp=gpar(fontsize=20)))



## aveRes 

g1 <- ggplot(data=useful,aes(x=texter_rating,y=aveRes)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("Response Time(Secs)") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=aveRes,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=aveRes,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=aveRes,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Response Time(Secs)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Average Counselor Response Time and Texter Rating",gp=gpar(fontsize=20)))



## severity of issues and texter rating

mosaicplot(~ severity + texter_rating, data = useful, color = c("#93C91C","#80827D","#F51857"),
           main="Severity of Issues and Texter Rating",xlab="Severity Level",ylab="Texter Rating")


## texter finish answering system message time

useful.trim <- useful[useful$texter_response<quantile(useful$texter_response,0.99),]
g1 <- ggplot(data=useful.trim,aes(x=texter_rating,y=texter_response)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("Response Time(Secs)") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.trim[useful$texter_rating=="better",],aes(x=texter_response,fill="better"),alpha=0.5) +
  stat_density(data=useful.trim[useful$texter_rating=="same",],aes(x=texter_response,fill="same"),alpha=0.5) +
  stat_density(data=useful.trim[useful$texter_rating=="worse",],aes(x=texter_response,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Response Time(Secs)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Texter Finish Answering System Question Time and Texter Rating",gp=gpar(fontsize=20)))


names(textVar)
names(useful)

useful <- merge(textVar,useful,by.x="conv_id",by.y="conv_id")

# qs ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=qsratio)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=qsratio,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=qsratio,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=qsratio,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))


# whyqs ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=sqrt(whyqs+1))) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=sqrt(whyqs+1),fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=sqrt(whyqs+1),fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=sqrt(whyqs+1),fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Transformed Why Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Why Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))

 # equal size
g1 <- ggplot(data=useful.sample,aes(x=texter_rating,y=sqrt(whyqs+1))) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.sample[useful.sample$texter_rating=="better",],aes(x=sqrt(whyqs+1),fill="better"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="same",],aes(x=sqrt(whyqs+1),fill="same"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="worse",],aes(x=sqrt(whyqs+1),fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Transformed Why Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Why Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))

# openqs ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=openqs)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=openqs,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=openqs,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=openqs,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Open-End Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Open-End Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))

 # equal size
g1 <- ggplot(data=useful.sample,aes(x=texter_rating,y=openqs)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.sample[useful.sample$texter_rating=="better",],aes(x=openqs,fill="better"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="same",],aes(x=openqs,fill="same"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="worse",],aes(x=openqs,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Open-End Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Open-End Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))

# closeqs ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=closeqs)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=closeqs,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=closeqs,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=closeqs,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Close-Ended Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Close-Ended Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))

# equal size
g1 <- ggplot(data=useful.sample,aes(x=texter_rating,y=closeqs)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.sample[useful.sample$texter_rating=="better",],aes(x=closeqs,fill="better"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="same",],aes(x=closeqs,fill="same"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="worse",],aes(x=closeqs,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Close-Ended Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Close-Ended Question to Statement Ratio and Texter Rating",gp=gpar(fontsize=20)))



## question to question ratio

# whyqq ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=sqrt(whyqq+1))) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=sqrt(whyqq+1),fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=sqrt(whyqq+1),fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=sqrt(whyqq+1),fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Transformed Why Question to Total Question Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Why Question to Total Question Ratio and Texter Rating",gp=gpar(fontsize=20)))

# equal size
g1 <- ggplot(data=useful.sample,aes(x=texter_rating,y=sqrt(whyqq+1))) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.sample[useful.sample$texter_rating=="better",],aes(x=sqrt(whyqq+1),fill="better"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="same",],aes(x=sqrt(whyqq+1),fill="same"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="worse",],aes(x=sqrt(whyqq+1),fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Transformed Why Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Why Question to Total Question Ratio and Texter Rating",gp=gpar(fontsize=20)))

# openqs ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=openqq)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=openqq,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=openqq,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=openqq,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Open-End Question to Statement Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Open-End Question to Total Question Ratio and Texter Rating",gp=gpar(fontsize=20)))

# equal size
g1 <- ggplot(data=useful.sample,aes(x=texter_rating,y=openqq)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.sample[useful.sample$texter_rating=="better",],aes(x=openqq,fill="better"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="same",],aes(x=openqq,fill="same"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="worse",],aes(x=openqq,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Open-End Question to Total Question Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Open-End Question to Total Question Ratio and Texter Rating",gp=gpar(fontsize=20)))

# closeqs ratio
g1 <- ggplot(data=useful,aes(x=texter_rating,y=closeqq)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=closeqq,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=closeqq,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=closeqq,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Close-Ended Question to Total Question Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Close-Ended Question to Total Question Ratio and Texter Rating",gp=gpar(fontsize=20)))

# equal size
g1 <- ggplot(data=useful.sample,aes(x=texter_rating,y=closeqq)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful.sample[useful.sample$texter_rating=="better",],aes(x=closeqq,fill="better"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="same",],aes(x=closeqq,fill="same"),alpha=0.5) +
  stat_density(data=useful.sample[useful.sample$texter_rating=="worse",],aes(x=closeqq,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Close-Ended Question to Total Question Ratio") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Close-Ended Question to Total Question Ratio and Texter Rating",gp=gpar(fontsize=20)))



# robotic messages

if(F){
g1 <- ggplot(data=useful,aes(x=texter_rating,y=robo)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=robo,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=robo,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=robo,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Number of Robotic Messages Used") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Number of Robotic Messages Used and Texter Rating",gp=gpar(fontsize=20)))
}


# I
g1 <- ggplot(data=useful,aes(x=texter_rating,y=I)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=I,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=I,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=I,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Times of Using I") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Times of Counselor Using I and Texter Rating",gp=gpar(fontsize=20)))



# exclamation mark

g1 <- ggplot(data=useful,aes(x=texter_rating,y=excla)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=excla,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=excla,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=excla,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Numbers of Exclamation Marks") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Numbers of Exclamation Marks Used and Texter Rating",gp=gpar(fontsize=20)))



# deep dive on exclamation mark

g1 <- ggplot(data=useful,aes(x=texter_rating,y=exclaratio)) +
  geom_point(position="jitter",aes(color=texter_rating),alpha=0.4,size=2.5) + geom_boxplot(outlier.size=0) +
  ggtitle("Boxplot") + ylab("") + xlab("") +
  coord_flip() + theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  theme(axis.text=element_text(size=rel(1.25))) +
  theme(legend.position="none") +
  scale_color_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10))

g2 <- ggplot() + stat_density(data=useful[useful$texter_rating=="better",],aes(x=exclaratio,fill="better"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="same",],aes(x=exclaratio,fill="same"),alpha=0.5) +
  stat_density(data=useful[useful$texter_rating=="worse",],aes(x=exclaratio,fill="worse"),alpha=0.5) +
  ggtitle("Density Plot") + ylab("") + xlab("Exclamation Mark Matching Score") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(2),face="bold")) +
  theme(axis.title=element_text(size=rel(1.25))) +
  scale_fill_manual(name="Texter Rating",values=cols) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

grid.arrange(g1,g2,ncol=1,main=textGrob("Exclamation Marks Matching Score and Texter Rating",gp=gpar(fontsize=20)))

save(useful,file="usefulVariable_c.Rda")
