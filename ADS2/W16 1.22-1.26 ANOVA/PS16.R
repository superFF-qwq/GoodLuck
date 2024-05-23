group1 <- rnorm(30, 3, 1)
group2 <- rnorm(100, 4, 0.1)
group3 <- rnorm(30, 5, 0.01)
response <- c(group1, group2, group3)
group <- c(rep(1, 30), rep(2, 100), rep(3, 30))
data <- data.frame(group, response)
head(data)

data$group = as.factor(data$group)

model = aov(response ~ group, data)

plot(model, 2)

resid = {}
for(i in 1:nrow(data)){
  mean = mean(data[data$group == data[i, "group"], "response"])
  resid = c(resid, data[i, "response"] - mean)
}
qqnorm(resid)
qqline(resid, col = "red", lwd = 2)

q1 = quantile(resid, probs = 0.25)
q3 = quantile(resid, probs = 0.75)
IQR = IQR(resid)
lb = q1 - 1.5 * IQR
rb = q3 + 1.5 * IQR
index = which(resid < lb | resid > rb)
index