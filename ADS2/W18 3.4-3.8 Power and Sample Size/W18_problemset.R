setwd("E:\\A大学\\大二下\\ADS2\\W18 Power and Sample Size")

### task1
# NOTE: in the task1 description, it tells us to use a two-sided test !!!
power.t.test(power = 0.8, delta = 0.25, sd = 0.42, sig.level = 0.05,
             type = "two.sample", alternative = "two.sided")
# confirm by checking n = 45 and n = 46
power.t.test(n = 45, delta = 0.25, sd = 0.42, sig.level = 0.05,
             type = "two.sample", alternative = "two.sided")
power.t.test(n = 46, delta = 0.25, sd = 0.42, sig.level = 0.05,
             type = "two.sample", alternative = "two.sided")

# You require 46 students in each group (heavy vs non-heavy drinkers).

### task2
## 2.1
power.t.test(n = 10, delta = 0.4, sd = 0.5, sig.level = 0.05,
             type = "two.sample", alternative = "two.sided")
# power = 0.3949428
## 2.2
power.t.test(n = 10, delta = 0.4, sd = 0.5, sig.level = 0.1,
             type = "two.sample", alternative = "two.sided")
# power = 0.5303875
# It has therefore increased when we increased the significance level.

## 2.3
power.t.test(n = 5, delta = 0.4, sd = 0.5, sig.level = 0.1,
             type = "two.sample", alternative = "two.sided")
# power = 0.31333
# The power has been reduced when we reduced the sample size
# (despite keeping the standard deviation constant).

## 2.4
power.t.test(n = 5, delta = 0.8, sd = 0.5, sig.level = 0.1,
             type = "two.sample", alternative = "two.sided")
# power = 0.7464331
## 2.5
# 2.5.1 significance level
px = (1:100)*0.005
py = {}
for(alpha in px)
  py = c(py, power.t.test(n = 5, delta = 0.8, sd = 0.5, sig.level = alpha,
                          type = "two.sample", alternative = "two.sided")$power)
plot(px, py, xlab = "significance level", ylab = "power",
     main = "power vs. significant level", ylim = c(0, 1.0))
# 2.5.2 sample size
px = 3:102
py = {}
for(size in px)
  py = c(py, power.t.test(n = size, delta = 0.8, sd = 0.5, sig.level = 0.05,
                          type = "two.sample", alternative = "two.sided")$power)
plot(px, py, xlab = "sample size", ylab = "power",
     main = "power vs. sample size", ylim = c(0, 1.0))

# 2.5.3 effect size
# px = (1:100)*0.01
px = seq(0, 10, by = 0.1)
py = {}
for(ef in px)
  py = c(py, power.t.test(n = 5, delta = ef, sd = 0.5, sig.level = 0.05,
                          type = "two.sample", alternative = "two.sided")$power)
plot(px, py, xlab = "effect size", ylab = "power",
     main = "power vs. effect size", ylim = c(0, 1.0))


### task3
## 3.1
sampleA = rnorm(n = 5, mean = 10, sd = 5)
sampleB = rnorm(n = 5, mean = 11, sd = 5)
t.test(sampleA, sampleB, alternative = "two.sided", paired = F)
## 3.2
sampleA = rnorm(n = 500, mean = 10, sd = 5)
sampleB = rnorm(n = 500, mean = 11, sd = 5)
t.test(sampleA, sampleB, alternative = "two.sided", paired = F)
## 3.3
set.seed(13)
# sample_sizes = seq(2, 2000, by = 10)
# px = seq(5,800,5)
px = seq(2, 2000, by = 10)
py = {}
for(size in px){
  sampleA = rnorm(n = size, mean = 10, sd = 5)
  sampleB = rnorm(n = size, mean = 11, sd = 5)
  py = c(py, 
         t.test(sampleA, sampleB)$p.value)
}
plot(px, py, xlab = "sample size", ylab = "p-value", cex = 0.5)
abline(h = 0.05, col = "red")

# Conclusion from problemset notes.
# You can see that, overall, the p-value decreases as the sample size increases.
# Once I have a sample size above ~250, almost all my simulations show
# a significantly different mean value (t-test, p < 0.05) between samples A and B.
# Once I have a sample size above ~250 !!! I think this is due to the seed!!!
