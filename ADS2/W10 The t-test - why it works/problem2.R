#getwd()
setwd("E:\\A大学\\大二上\\ADS2\\week10 11.20-11.26")
temp=read.csv(file="OrionTemp.csv")
x=1:1000
y=NULL
tot=0
for(i in x){
  mysample=sample(temp$Temperature,size=10)
  if(t.test(mysample,mu=37,alternative="two.sided")$p.value<=0.05)
    tot=tot+1
  y=c(y,tot)
}
plot(x,y,type="l",
     xlab="Number of tests",ylab="Number of tests with p<=0.05",
     main="One-sample t-test, sample size=10")
#null distribution: the distribution if the null hypothesis is true