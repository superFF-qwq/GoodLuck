library(ggplot2)
library(dplyr)
substance_use <- read.csv("/Users/wangxiaolei/Desktop/ADS Group ICA/substance_use.csv")
anyNA(substance_use)
head(is.na(substance_use))
tail(substance_use)
tail(is.na(substance_use))
apply(is.na(substance_use), 2, which)
dim(substance_use)
data.noNA = substance_use[complete.cases(substance_use), ] 
dim(data.noNA)
duplicated(data.noNA)
frw.idx = which(duplicated(data.noNA)) 
rvs.idx = which(duplicated(data.noNA, fromLast = TRUE))
data.noNA[c(frw.idx, rvs.idx), ]
dim(data.noNA)
data.noNA.noDup = data.noNA[!duplicated(data.noNA),] 
dim(data.noNA.noDup)
head(data.noNA.noDup)
plot(x = data.noNA.noDup$year, y = data.noNA.noDup$val, pch = 20, col = "darkgoldenrod4",
     las = 1, xlab = "year", ylab = "val",
     main = "year~val", bty = "l")
text(data.noNA.noDup$year, data.noNA.noDup$cal, labels = data.noNA.noDup$X, col = "dimgray", cex = 0.7, pos = 4)