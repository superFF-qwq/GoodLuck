men=rnorm(45,mean=172,sd=7)
women=rnorm(55,mean=158.5,sd=6)
#print(men)
#print(women)
png("boxplot.png")
bp=boxplot(men,women,names=c("Men","Women"),
           border="purple",col="lightyellow",
           xlab="Groups",ylab="Height",main="Height of Students")
dev.off()