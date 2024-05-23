library(knitr)
library(tidyverse)
library(cowplot)
setwd("E:\\A??????\\?????????\\ADS2\\Week 14 Preparing for the open-book coding challenge\\Practical 14 Mock coding challenge")

stroop <- read.csv(file = "stroop_test.csv")

stroop$Time = as.factor(stroop$Time)
g = ggplot(data = stroop,
           mapping = aes(x = Time, y = Score, color = Time)) +
  geom_histogram()
g
hist(
  x = stroop[stroop$Time == "Morning", ]$Score,
  xlab = "Score",
  main = NA,
  freq = seq()
)
marathon = read.csv(file = "Chicago2013_random_finishers.csv")
marathon$Gender = as.factor(marathon$Gender)
max(marathon$Age)
min(marathon$Age)
marathon$Age
marathon$Age = cut(marathon$Age, breaks = c(18, 30, 41, 52, 63))
marathon$Age
g = ggplot(data = marathon,
           mapping = aes(x = Age, y = Time, color = Gender)) +
  geom_boxplot() +
  labs(title = "time of amateur runners by age and gender")
g
data = marathon[marathon$Gender == "F", ]$Time
mean(data)
sd(data)
aggregate(Time ~ Gender, data = marathon, FUN = mean)
aggregate(Time ~ Gender, data = marathon, FUN = sd)
aggregate(Time ~ Age, data = marathon, FUN = mean)
