library(ggplot2)
library(dplyr)
library(zoo)
library(tidyverse)
library(tidyr)
library(knitr)

## task 1

# Bayes Factor = P(D|H1) / P(D|H2) = 0.7 / 0.4 = 1.75

7/4

pd1 = seq(0.1, 1, by = 0.1)
pd2 = seq(0.1, 1, by = 0.1)

df = {}

library(ggplot2)
library(dplyr)
library(zoo)
library(tidyverse)
library(tidyr)
library(knitr)

## task 1: Use my priors as described below to determine
## the posterior probability that enough barley is being added to each pint.

# p(H|D) = p(D|H) * p(H) / p(D)
# p(H|D) posterior
# p(H) prior

# P(H1) = P(enough barley) = 0.5
# P(H2) = P(not enough barley) = 0.5
# P(DATA|H1) = P(mean barley content 46g|enough barley) = 0.7
# P(DATA|H2) = P(mean barley content 46g|not enough barley) = 0.4

# so posterior H1 = enough barley
# P(H1|D) = p(D|H1) * p(H1) / p(D)
# From this formula, we need to calculate p(D)
# p(D) = p(D|H1)*P(H1) + p(D|H2)*P(H2) = 0.7*0.5 + 0.4*0.5 = 0.55
# P(H1|D) = p(D|H1) * p(H1) / p(D) = 0.7 * 0.5 / 0.55 = 7/11

0.7*0.5/0.55

# Bayes Factor = P(D|H1) / P(D|H2) = 0.7 / 0.4 = 1.75

7/4

## task 2

pd1 = seq(0.1, 1, by = 0.1)
pd2 = 1 - pd1

# why pd1 + pd2 = 1???

df = data.frame(pd1 = pd1, pd2 = pd2, bf = pd1/pd2)

df = df %>%
  mutate(bf = ifelse(bf == "Inf", 10, bf))

df

# summary(df)
# df
ggplot(df, mapping = aes(x = pd1, y = pd2, size = bf, color = bf)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(0, 10)) +
  labs(x = 'P(DATA | H1)', y = 'P(DATA | H2)',
       size = "Bayes Factor", color = "")

ggplot(df, mapping = aes(x = pd1, y = pd2)) +
  theme_classic() +
  geom_point(size = 4, alpha = 0.2) +
  geom_line(aes(color = bf), size = 1) +
  scale_color_gradient(low = "orange", high = "darkred") + # , name = ""
  # set the minimum and maximum gradient color
  labs(x = 'P(DATA | H1)', y = 'P(DATA | H2)',
       color = "Bayes Factor")

## task 3

# p(H1) = 0.2
# p(H2) = 0.8

# based on Bayes Factor = P(D|H1) / P(D|H2)
# change in prior p(H1) does NOT affect the Bayes factor

# p(D) = p(D|H1)*P(H1) + p(D|H2)*P(H2) = 0.7*0.2 + 0.4*0.8 = 0.46
0.7*0.2 + 0.4*0.8
# P(H1|D) = p(D|H1) * p(H1) / p(D) = 0.7 * 0.2 / 0.46 = 14/46 = 7/23 = 0.3043478
0.7 * 0.2 / 0.46
# p(H1) decrease causes posterior decrease

## task 4 A dice game

# H1 = only 1 six
# H2 H3 H4 H5 H6

# p(D|H1)
pd1 = dbinom(7, size = 20, prob = 1/6)
# p(D|H2)
pd2 = dbinom(7, size = 20, prob = 2/6)
# p(D|H3)
pd3 = dbinom(7, size = 20, prob = 3/6)
# p(D|H4)
pd4 = dbinom(7, size = 20, prob = 4/6)
# p(D|H5)
pd5 = dbinom(7, size = 20, prob = 5/6)
# p(D|H6)
pd6 = dbinom(7, size = 20, prob = 6/6)

pd1
pd2
pd3
pd4
pd5
pd6
which.max(c(pd1,pd2,pd3,pd4,pd5,pd6))

## the practical std said that the priors
## are the following p(H1) = 0.96 P(H2) = p(H3) = p(H4) = p(H5) = 0.01
## Thoretically, p(Hi) = priors, these are designed by yourself

p2 = p3 = p4 = p5 = 0.2
p6 = 0
p1 = 1 - p2 - p3 - p4 - p5 - p6

# p(D) = sigma p(D|Hi) * p(Hi) 
pd = pd1*p1 + pd2*p2 + pd3*p3 + pd4*p4 + pd5*p5 + pd6*p6
pd
# p(H1|D) = p(D|H1) * p(H1) / p(D)
p1d = pd1*p1/pd
p2d = pd2*p2/pd
p3d = pd3*p3/pd
p4d = pd4*p4/pd
p5d = pd5*p5/pd
p6d = pd6*p6/pd

p1d # The answer is consistent with that in the practical solution.
p2d
p3d
p4d
p5d
p6d
which.max(c(p1d,p2d,p3d,p4d,p5d,p6d))
# the practical notes said that to compare which hypothesis is the most
# probable, we should compare p(Hi|D)

