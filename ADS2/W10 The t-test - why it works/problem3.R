#getwd()
setwd("E:\\A大学\\大二上\\ADS2\\week10 11.20-11.26")
temp=read.csv(file="OrionTemp.csv")
#t.test(temp$Temperature,mu=37,alternative="two.sided")
# p>0.05 We cannot reject null hypothesis. The data provide
# insufficent envidence that the mean is not equal to 37.
rate=NULL
for(size in 5:100){
  error=0
  times=10000
  for(i in 1:times){
    #size=runif(1,min=5,max=100)
    mysample=sample(x=temp$Temperature,size)
    if(t.test(mysample,mu=37,alternative="two.sided")$p.value>0.05)
      error=error+1
  }
  rate=c(rate,1.0*error/times)
}
plot(5:100,rate,xlab="sample size",ylab="error rate",type="l",
     main="percentage of erroneous results with various sample sizes")
