setwd("E:\\A大学\\大二上\\ADS2\\week10 11.20-11.26")
file=read.table(file="barley.txt")
length(file$V1)
#head(file)
#summary(file)
#null hypothesis: The brewery is adding enough barley. mean>=mu
#alternative hypothesis: The brewery is not adding enough barley. mean<mu
t.test(file$V1,mu=50,alternative="less")
#p-value < 0.05 we reject null hypothesis