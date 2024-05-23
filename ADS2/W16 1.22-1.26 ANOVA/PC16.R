library(tinytex)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(cowplot)
setwd("E:\\A大学\\大二下\\ADS2\\W16 1.22-1.26 ANOVA")

## problem 1: Drug trial dataset - revisited
trial = read.csv("drug_trial.csv")
head(trial)
# H0: There is no effect of drugA or drugB on pain
# HA: At least one of these factors (drugA or drugB) influences pain.
summary(trial)
trial$treatment = as.factor(trial$treatment)
g = ggplot(data = trial, mapping = aes(x = treatment, y = pain, color = treatment))
g = g + geom_boxplot()
g
# assumptions for ANOVA
# 1. independent random sampling
# 2. normality of residues
# 3. equality of variances

# one-way ANOVA
model = aov(pain~treatment, data = trial)

# verification of assumption 2: normality of residues
# method 1: Plot histogram of residuals and use visual inspection
hist(resid(model), main = "residuals")
# method 2: Use one of the analytic plots provided by R when using aov and
# eyeball it.  The plot is the second one shown and is called Normal Q-Q.
# Dots should be aligned along the diagonal.
plot(model, 2)
# method 3: Use a formal test for normality, e.g. the Shapiro-Wilk test
shapiro.test(resid(model))
summary(model)
resid(model)
nrow(trial)

# verification of assumption 3: equality of variances
plot(model, 1)

summary(model)
# *** represents p<0.001

TukeyHSD(model)
# The results of multiple comparisons are output in a table format, 
# showing the mean difference (diff), confidence interval (lwr and upr), 
# and adjusted p-value (p adj) between different treatment methods.

## problem 2: Weight loss in mice
mouse = read.csv("mouse_experiment.csv")
head(mouse)
summary(mouse)
mouse$genotype = as.factor(mouse$genotype)
mouse$diet = as.factor(mouse$diet)
g2 = ggplot(data = mouse)
g2 = g2 + geom_boxplot(mapping = aes(x = genotype,
                                y = weight_gain,
                                fill = diet))
g2
bartlett.test(pain ~ treatment, data = trial)

# H0: There is no interaction

# 2-way ANOVA
model2 = aov(weight_gain ~ genotype * diet, data = mouse)
#plot(model2)

# assumption 2
# M1
hist(resid(model2), main = "Residuals", xlab = "Deviations")
# M2
plot(model2, 1)
# M3
shapiro.test(resid(model2))

# assumption 3
plot(model2, 1)

model2
summary(model2)

TukeyHSD(model2)

# bartlett test is not right for two-way ANOVA

## highly recommended that all group has equal size.
