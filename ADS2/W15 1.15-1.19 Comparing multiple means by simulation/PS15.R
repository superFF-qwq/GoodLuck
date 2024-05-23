setwd("E:\\A大学\\大二下\\ADS2\\W15 Comparing multiple means by simulation")
library(tinytex)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(cowplot)

### The problem with jelly beans
bean = read.csv("jellybeans.csv")
#head(bean, 50)
#tail(bean)
#summary(bean)
bean$colour = as.factor(bean$colour)
#summary(bean$colour)
#mean(bean$score)
#sd(bean$score)

length(unique(bean$colour))
# Answer: There will be 20 t-test.

1-0.95^20
# false positive rate

#boxplot(data = bean, score ~ colour)

if(F){ # a useful way to arrange the top one of the order
desired_level = "control"
all_level = levels(bean$colour)
all_level = c(desired_level, setdiff(all_level, desired_level))
bean$colour = factor(bean$colour, levels = all_level)
levels(bean$colour)
}

col = as.character(unique(bean$colour))
bean$colour = factor(bean$colour, levels = col)

col2 = gsub("control", "white", col)
col2 = gsub("teal", "#008080", col2)
col2 = gsub("mauve", "#DDA0DD", col2)
col2 = gsub("lilac", "#C8A2C8", col2)
col2 = gsub("peach", "#FFC0CB", col2)

g1 = ggplot(data = bean, mapping = aes(x = colour, y = score, fill = colour))
g1 = g1 + geom_boxplot()
g1 = g1 + title("Jelly beans")
g1 = g1 + scale_x_discrete(breaks = NULL)
g1 = g1 + scale_fill_manual(values = col2)
g1

# str_vector_replaced <- gsub(old_string, new_string, str_vector)
# str_vector_replaced <- str_replace_all(str_vector, old_string, new_string)

#head(bean)
#control = bean[bean$colour == "control", ]$score
control = bean[bean$colour == "control", "score"]
purple = bean[bean$colour == "purple", "score"]
t.test(control, purple)

# Interpretation:
# Indeed, the t-test gives a positive result.
# (I know it is a false positive, because I generated the dataset
# myself by sampling from the same distribution for every colour,
# so in theory, there really is no difference)

### Can we look at variance instead?

# H0: All colors and the control the same effect on the acne effect.
# HA: There is at least one difference between the colors and the control on the
# acne effect.

# H0: The difference between two data from different groups is no larger than the
#     difference between two data from the same groups.
# HA: The difference between two data from different groups is larger than the
#     difference between two data from the same groups.

same = {}
diff = {}
# work = function(data, reptimes){
#   for(i in 1:reptimes){
#     col3 = sample(col, 2)
#     a1 = sample(bean[bean$colour == col3[1], "score"], 2)
#     a2 = sample(bean[bean$colour == col3[2], "score"], 1)
#     same = c(same, abs(a1[1] - a1[2]))
#     diff = c(diff, abs(a1[1] - a2))
#   }
#   str(same)
#   return (abs(mean(same) - mean(diff)))
# }
# our_exp = work(bean, 10000)
reptimes = 10000
for(i in 1:reptimes){
  col3 = sample(col, 2)
  a1 = sample(bean[bean$colour == col3[1], "score"], 2)
  a2 = sample(bean[bean$colour == col3[2], "score"], 1)
  same = c(same, abs(a1[1] - a1[2]))
  diff = c(diff, abs(a1[1] - a2))
}

hist(same)
hist(diff)

qqnorm(same)
qqline(same, col = "red", lwd = 2)

qqnorm(diff)
qqline(diff, col = "red", lwd = 2)

wilcox.test(same, diff, alternative = "less")
# p > 0.05 so H0 is not rejected.

model = aov(data = bean, score ~ colour)

# assumptions for ANOVA
# 1. independent random sampling
# 2. normality of residues
# 3. equality of variances

# For assumption 2
hist(resid(model))
plot(model, 2)

# For assumptio 3
plot(model, 1)

summary(model)
data = TukeyHSD(model)$colour[, "p adj"]
data
str(data)
data[data < 0.05]

### There is a difference, what now?
# 1-(1-a)^6=0.05
1-0.95^(1/6)
# alpha_adjusted
