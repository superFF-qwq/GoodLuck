library(tinytex)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(cowplot)
setwd("E:\\A大学\\大二下\\ADS2\\W15 Comparing multiple means by simulation")
trial = read.csv("drug_trial.csv")
head(trial)
str(trial)
trial$treatment = as.factor(trial$treatment)
str(trial)
g = ggplot(data = trial, mapping = aes(x = treatment, y = pain, color = treatment))
g = g + geom_boxplot()
g
# H0: Drug A, drug B and placebo has the same effect on the pain.
# HA: There is at least one difference between the effect of drug A, drug B
#     and placebo on the pain.

# H0: The difference between two data from different groups is no larger than the
#     difference between two data from the same groups.
# HA: The difference between two data from different groups is larger than the
#     difference between two data from the same groups.

# randomly draw two data points
gsam = c()
gdif = c()
for(i in 1:100){
  sample_index = sample(1:nrow(trial), 2)
  x = trial[sample_index[1], ]
  y = trial[sample_index[2], ]
  dif = abs(x$pain - y$pain)
  if(x$treatment == y$treatment)
    gsam = c(gsam, dif)
  else
    gdif = c(gdif, dif)
}
data = data.frame(c(rep("sample group", length(gsam)),
                    rep("different groups", length(gdif))),
                  c(gsam, gdif))
names(data)[1] = "type"
names(data)[2] = "difference"
data$type = as.factor(data$type)
head(data)
tail(data)
summary(data)
g2 = ggplot(data = data, mapping = aes(x = type, y = difference))
g2 = g2 + geom_boxplot(mapping = aes(fill = type))
g2
mean(gsam)
mean(gdif)
mean(gdif) - mean(gsam)
wilcox.test(data$difference ~ data$type, alternative = "greater")
wilcox.test(x = data[data$type == "different groups", "difference"],
            y = data[data$type == "sample group", "difference"],
            alternative = "greater")

g3 = 
ggplot(data = data) +
  geom_line(mapping = aes(x = difference, col = type), stat = "density")
g3
  

# p-value is much smaller than 0.05
# we reject null hypothesis
# HA: The difference between two data from different groups is larger than the
#     difference between two data from the same groups.

# Questions:
# A t-test is not a good idea here (why not?)

# If we pick large enough data points, the distribution of the two type groups
# must be normal.
# You can simply look at the boxplot to determine whether it is normal. 
# Whether it is relatively symmetric?
# Prof. Young said that it is not ideal to use the t-test because of the variances
# of the two type groups are different.
# It is better to use wilcox test to compare the medians.

# Also, what information do you think is still missing?
# Up to now, we just know that the difference in the "different groups" is bigger
# than the difference in the "same group".
# BUT we still do not know does the drug A/B has a postive/negative effect.

# Why do we not use t-test to compare the effect of two drugs?
# Because what is the reference that you choose the exact two drug.
# You are expected not to have previous expectations.
# After looking at the data (boxplot) of different groups, you formulate the 
# hypothesis, it is just like cherry picking.
# cherry picking: 挑三拣四：在选择中只挑选出有利的项目，而拒绝不利的项目，
# 以获取优势或将某事物呈现在最好的光线下的行为。
# We must set parameters (hypothesis) before seeing the data.
# After ANOVA, may apply modified t-test.
