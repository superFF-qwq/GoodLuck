setwd("E:\\A大学\\大二下\\ADS2\\W19 Analysing categorical data")

library(tinytex)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(cowplot)

### 1. Simulation of the probability of goodness-of-fit test
Poll_seasons <- data.frame(Spring = 40, Summer = 30, Autumn = 18, Winter = 28)

equal_preferences <- sum(Poll_seasons) * 0.25

reptimes = 1e5
data = {}
for(i in 1:reptimes){
  sample = sample(1:4, 116, replace = T)
  now = {}
  for(j in 1:4)
    now = c(now, sum(sample == j))
  data = c(data, sum((now - equal_preferences)^2/equal_preferences))
}
plot(density(data), xlim = c(0,25), main = "df = 3 chi-square distribution")
squ = sum((Poll_seasons - equal_preferences)^2/equal_preferences)
den = density(data)
x_index <- which.min(abs(den$x - squ))
if (x_index == length(den$x)) {
  prob_right <- 0
} else {
  # 计算x = squ右侧所有小区间的面积之和
  prob_right <- sum(den$y[(x_index+1):length(den$x)])*(den$x[2] - den$x[1])
}

prob_right

chisq.test(Poll_seasons, p = rep(1/4, 4))
chisq.test(Poll_seasons)

###  Chi-square distribution and degree of freedom

df <- c(1, 2, 3, 4, 6, 9)

data2 = {}

# for (i in 1:length(df)) { 
#   data = density(rchisq(1e4, df = df[i]))
#   data2 = rbind(data2, data.frame(x = data$x, y = data$y, group = factor(i)))
# }
# 
# g = ggplot(data = data2, mapping = aes(x = x, y = y, color = group)) 
# g = g + geom_line()
# g = g + xlim(0, 8)
# g

tot = 1e4
for (i in 1:length(df))
  data2 = c(data2, rchisq(tot, df = df[i]))

data2.2 = data.frame(df = rep(df, each = tot), value = data2)
g2 = ggplot(data = data2.2, mapping = aes(x = value, color = factor(df)))
g2 = g2 + geom_density(size = 1)
g2 = g2 + xlim(0, 8)
g2

### 3. Chi-square test of homogeneity

Severe <- data.frame(
  Spring = 5,
  Summer = 1,
  Fall = 1,
  Winter = 9
)
Mild <- data.frame(
  Spring = 8,
  Summer = 5,
  Fall = 2,
  Winter = 5
)
Sporadic <- data.frame(
  Spring = 9,
  Summer = 8,
  Fall = 3,
  Winter = 9
)
Never <- data.frame(
  Spring = 18,
  Summer = 16,
  Fall = 12,
  Winter = 5
)
Two_categories <- rbind(Severe, Mild, Sporadic, Never)
rownames(Two_categories) = c("Severe", "Mild", "Sporadic", "Never")
chisq.test(Two_categories)

Two_categories = as.matrix(Two_categories)

data = data.frame(
  season = rep(c("Spring", "Summer", "Autumn", "Winter"), each = 4),
  allergy = rep(c("Severe", "Mild", "Sporadic", "Never"), times = 4),
  count = c(5, 8, 9, 18, 1, 5, 8, 16, 1, 2, 3, 12, 9, 5, 9, 5)
)
data$allergy = as.factor(data$allergy)
levels(data$allergy) = c("Never","Sporadic","Mild","Severe")
g3 = ggplot(data, mapping = aes(x = allergy, y = count, fill = season))
g3 = g3 + geom_bar(stat = "identity", position = "stack")
g3

mat = matrix(data$count, nrow = 4, byrow = T,
             dimnames = list(c("Spring", "Summer", "Autumn", "Winter"),
                             c("Severe", "Mild", "Sporadic", "Never")))
mosaicplot(mat, color = c("green", "pink", "orange", "skyblue"),
           xlab = "Favorite season", ylab = "Observed allergic response",
           main = "")

g3.2 = ggplot(data, mapping = aes(x = season, y = allergy, size = count))
g3.2 = g3.2 + geom_point(shape = 21, mapping = aes(fill = season))
g3.2 = g3.2 + scale_size_area(max_size = 15)
# enlarge all the balloons by proportion.
g3.2 = g3.2 + labs(title = "balloons")
g3.2 = g3.2 + theme(plot.title = element_text(hjust = 0.5)) 
g3.2

### 4. Chi-square test and Fisher’s exact test
data4 <- matrix(c(7,2,3,7), nrow = 2, byrow = T,
                dimnames = list(c("Alive","Dead"), c("WT","KO")))
chisq.test(data4)
chisq.test(data4, correct = F)
fisher.test(data4)
