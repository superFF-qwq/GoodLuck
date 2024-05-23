#getwd()
setwd("E:\\A大学\\大二上\\ADS2\\week13 12.11-12.17")
#getwd()
if(F){
library(dplyr)

readall<-list.files(path = "D:test",
                    pattern = "*.csv",
                    full.names = T)
readall%>%
  lapply(read.csv)%>%
  bind_rows -> merge
write.csv(merge, file="teaching.csv")
}
if(F){
data1=read.csv("data1.csv")
data2=read.csv("data2.csv")
head(data1)
data=rbind(data1,data2)
summary(data)
}
readall=list.files(pattern="*.csv")
readall
data=NULL
for(file in readall){
  data0=read.csv(file)
  data=rbind(data,data0)
}
#summary(data)
#head(data)
data$Time_Point=as.factor(data$Time_Point)
#summary(data)
#mode(data$Time_Point)
hist(data$Pain_Level,breaks=seq(0,10,0.5))
library(tidyverse)
library(cowplot)
g=ggplot(data=data,mapping=aes(x=Time_Point,y=Pain_Level,color=Time_Point))
g+geom_boxplot()
l1=data[data$Time_Point=="Immediate",]$Pain_Level
l2=data[data$Time_Point=="3 Weeks",]$Pain_Level
#l1
#l2
#length(l1)
#length(l2)
n=min(length(l1),length(l2))
l1=sample(l1,n)
l2=sample(l2,n)
t.test(l1,l2,alternative="greater",paired=T)