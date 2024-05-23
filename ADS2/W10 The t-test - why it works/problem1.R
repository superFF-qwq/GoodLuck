#getwd()
setwd("E:\\A大学\\大二上\\ADS2\\week10 11.20-11.26")
temp=read.csv(file="OrionTemp.csv")
#head(temp)
#summary(temp)
#length(temp$Temperature)
mysample=sample(temp$Temperature,size=10)
#mysample
data=t.test(mysample,mu=37,alternative="two.sided")
tvalue=abs(data$statistic)
#tvalue
x=seq(-10,10,by=0.01)#length=100000
df=length(mysample)-1
y=dt(x=x,df=df)
plot(x,y,type="l",xlab="t value",ylab="p(t)",main="t distribution")
pvalue=pt(tvalue,df,lower.tail=FALSE)*2
pvalue
data$p.value
#mode(tvalue)
#null distribution: the distribution if the null hypothesis is true