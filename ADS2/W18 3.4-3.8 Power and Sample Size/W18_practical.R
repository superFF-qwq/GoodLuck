setwd("E:\\A大学\\大二下\\ADS2\\W18 Power and Sample Size")

### task1
# The assumption is the student is also normally distributed and sd = 10.
ans = {}
# H0: the height <= 175
# HA: the height > 175 (HA must be true)
for(i in 1:1e4){
  sample = rnorm(10, 178, 10)
  ans = c(ans, t.test(sample, mu = 175, alternative = "greater")$p.value)
}
# print(ans)
count = sum(ans < 0.05)
#ans > 0.05
#length(ans > 0.05)
#ans
# count
# HA is true: the height < 178
# count is where we accept H0
P0 = count / 1e4
P0
# P0 is the power

count2 = 0
for(i in 1:1e4){
  if(t.test(rnorm(50, 178, 10), mu = 175, alternative = "greater")$p.value < 0.05)
    count2 = count2 + 1
}
P2 = count2 / 1e4
P2

plot_x = seq(5, 100, 5)
plot_x
plot_y = {}
for(ns in plot_x){
  plot_y = c(plot_y,
            power.t.test(n = ns, delta = 178 - 175, sd = 10, sig.level = 0.05,
               type = "one.sample", alternative = "one.sided")$power)
}
plot_y
plot(x = plot_x, y = plot_y,
     #xlim = c(0, 100), ylim = c(0.0, 1.0),
     xlab = "Ns", ylab = "Powers",
     type = "b")
# type
# "p"：只绘制点（默认值）
# "l"：只绘制线条
# "b"：同时绘制点和线条
# "o"：在数据点上方绘制线条，并且绘制点
# "h"：绘制垂直线从点到x轴
# "s"：绘制阶梯图
abline(h = 0.8, col = "red", lwd = 0.2)
# lwd 线的宽度

###task2
# question 2.1 NOTE two groups
# H0: is not effective weight >= 130
# HA: is effective weight < 130
cnt = 0
for(i in 1:1e4){
  if(t.test(x = rnorm(10, 130 * 0.9, 30), 
            y = rnorm(10, 130, 30),
            alternative = "less")$p.value
     > 0.05) # do not reject H0, which means do not see an effect
    cnt = cnt + 1
}
Pr = cnt / 1e4
Pr # type2 error
power = 1 - Pr
power
# power < 80% is not good enough in industry

# question 2.2
# let type2 error < 0.2, so power >= 0.8
power.t.test(power = 0.8, delta = 130 * 0.1, sd = 30, sig.level = 0.05,
             type = "two.sample", alternative = "one.sided")
# confirm by checking n = 66 and n = 67
power.t.test(n = 66, delta = 130 * 0.1, sd = 30, sig.level = 0.05,
             type = "two.sample", alternative = "one.sided")
power.t.test(n = 67, delta = 130 * 0.1, sd = 30, sig.level = 0.05,
             type = "two.sample", alternative = "one.sided")
# need to recruit 67 * 2 = 134

# question 2.3
power.t.test(power = 0.8, delta = 130 * 0.1, sd = 30, sig.level = 0.05,
             type = "paired", alternative = "one.sided")
# need to recruit 35

# question 2.4
px = seq(0.01, 0.05, 0.01)
py = {}
for(alpha in px){
  py = c(py, power.t.test(power = 0.8, delta = 130 * 0.1, sd = 30,
                                  sig.level = alpha, type = "paired",
                                  alternative = "one.sided")$n)
}
plot(px, py, xlab = "alphas", ylab = "Ns", type = "b")

px2 = seq(0.5, 0.9, 0.05)
py2 = {}
for(pows in px2){
  py2 = c(py2, power.t.test(power = pows, delta = 130 * 0.1, sd = 30,
                                  sig.level = 0.05, type = "paired",
                                  alternative = "one.sided")$n)
}
plot(px2, py2, xlab = "powers", ylab = "Ns2", type = "b")