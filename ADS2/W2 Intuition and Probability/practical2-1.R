chicago=read.csv("Chicago2013.csv")
#print(class(chicago))
#print(chicago)
country=unique(chicago$Country)
#print(country)
countrydata=matrix(ncol=2,nrow=0)
for(co in country){
  tot=sum(chicago$Country==co)
  countrydata=rbind(countrydata,matrix(data=c(co,tot),ncol=2))
}
colnames(countrydata)=c("Country","People")
countrydata=as.data.frame(countrydata)
countrydata$People=as.numeric(countrydata$People)
#print(countrydata)
# png("histo.png")
# hist(chicago$Time,main="Histogram of Finishing Times",freq=TRUE,
#            col="lightyellow",border="purple",
#            xlab="Times",ylab="number",
#            ylim=c(0,20))#,plot=FALSE
# dev.off()
sdata=sample(chicago$Time,10)
print(sdata)
png("histo1.png")
hist(sdata,main="Histogram of 10 Finishing Times",freq=TRUE,
     col="lightyellow",border="purple",
     xlab="Times",ylab="number",
     ylim=c(0,max(sdata)),plot=TRUE)
dev.off()