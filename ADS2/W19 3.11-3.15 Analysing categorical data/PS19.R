setwd("E:\\A大学\\大二下\\ADS2\\W19 3.11-3.15 Analysing categorical data")

library(tinytex)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(cowplot)

### task1: Candy production
observed = c(40, 32, 48, 57, 23)
chisq.test(observed)
# p-value = 0.001444 < 0.05
# reject H0
# should not produce the same amount of ice cream with different flavours

## 1.1 Simulation to draw df = 4
# H0: There candy preferences for each flavour are evenly distributed.
reptimes = 1e5
sum = sum(observed)
# sum
expected = rep(sum/5, times = 5)
# expected
data = {}
for(i in 1:reptimes){
  sample = sample(1:5, sum, replace = T)
  now = {}
  for(j in 1:5)
    now = c(now, sum(sample == j))
  data = c(data, sum((now - expected)^2/expected))
}

den = density(data)
plot(den, main = "df = 4 chi-square distribution", xlab = "x^2")
pos = sum((observed - expected)^2/expected)
# index = which.min(abs(den$x - pos))
# if(index == length(den$x)){
#   p_value = 0
# } else{
#   p_value = sum(den$y[(index+1):length(den$x)]) * (den$x[2] - den$x[1])
# }
# p_value
pos
sum(data >= pos) / length(data)

plot_chisq_distribution = function(data) {
  g = ggplot(data = data.frame(x = data), mapping = aes(x = x))
  g = g + geom_density()
  g = g + labs(x = "x^2",
               title = "df = 4 chi-square distribution")
  g = g + theme(plot.title = element_text(hjust = 0.5))
  g
}

plot_chisq_distribution(data)

## 1.2
# df = 4
df4 = rchisq(n = reptimes, df = 4)
plot_chisq_distribution(df4)
# rchisq: randomly generate n points on the chisq distribution

# if use ggplot2 to draw the two curve in one graph, it will be much clearer to
# compare the curve.

plot_df = data.frame(df4, simulation = data)
plot_df = gather(plot_df, key = "method", value = "chisq_value")
str(plot_df)
plot_df$method = as.factor(plot_df$method)
ggplot(data = plot_df, mapping = aes(x = chisq_value, color = method)) +
  geom_density()

### task2: Analyze the 3-way mouse data from the lecture.

## 2.1
data2 = array(data = 0, dim = c(2, 2, 2))
# dim1: WT/KO dim2: Alive/Dead dim3: Male/Female
data2[1, 1, 1] = 40
data2[1, 1, 2] = 34
data2[1, 2, 1] = 9
data2[1, 2, 2] = 7
data2[2, 1, 1] = 20
data2[2, 1, 2] = 25
data2[2, 2, 1] = 15
data2[2, 2, 2] = 20
data2

dname <- list(genotype = c("WT", "KO"),
              lifespan = c("Alive", "Dead"),
              sex = c("Male", "Female")
              )
dimnames(data2) = dname
data2

## 2.2

# H0: There is no interdependency among geneX, sex and lifespan of mice
# HA: There is interdependency among the variables

mice = as.table(data2)
mice
summary(mice)

total = sum(data2)
total

apply(data2, 1, sum)

expected_p = {}
for(i in 1:2)
  for (j in 1:2)
    for (k in 1:2) {
      s1 = apply(data2, 1, sum)
      s2 = apply(data2, 2, sum)
      s3 = apply(data2, 3, sum)
      if(sum(s1) != total | sum(s2)!= total | sum(s3) != total){
        print("FALSE!!!")
      }
      expected_p = c(expected_p,
                     s1[i] / total * s2[j] / total * s3[k] / total)
    }
expected_p
sum(expected_p)

expected_data2 = expected_p * total
expected_data2

observed_data2 = array(data2, dim = length(data2))
observed_data2

calc_df = function(r, c, l) {
  return (r * c * l - 1 - (r - 1) - (c - 1) - (l - 1))
}
chisq_value = sum((observed_data2 - expected_data2) ^ 2 / expected_data2)
chisq_value
calc_df(2, 2, 2)
pchisq(chisq_value, calc_df(2, 2, 2), lower.tail = F)
# lower.tail = T means calculate (-oo, q)
# p = 0.001608537 < 0.05 reject H0

library(rcompanion)
mat = matrix(c(7, 2, 3, 7), nrow = 2, byrow = T)
fisher.test(mat)

## 2.3
 
library(plot3D)
library(rgl) 
str(data2)

# x_coords <- rep(1:dim(data2)[1], each = dim(data2)[2] * dim(data2)[3])  
# y_coords <- rep(rep(1:dim(data2)[2], each = dim(data2)[3]), times = dim(data2)[1])  
# z_coords <- rep(1:dim(data2)[3], times = prod(dim(data2)[1:2]))  
# 
# 
# # 使用 points3D 绘制三维数组中的点  
# points3D(x = x_coords, y = y_coords, z = z_coords, col = "blue")

# x_coords <- rep(1:dim(data2)[1], each = dim(data2)[2] * dim(data2)[3])  
# y_coords <- rep(rep(1:dim(data2)[2], each = dim(data2)[3]), times = dim(data2)[1])  
# z_coords <- rep(1:dim(data2)[3], times = prod(dim(data2)[1:2]))  
# 
# # 创建一个颜色向量，基于数组的值进行着色  
# colors <- topo.colors(8)  # 生成8种不同的颜色  
# 
# # 打开一个新的3D图形窗口  
# open3d()  
# 
# # 使用 points3d 绘制三维数组中的点，并根据值着色  
# points3d(x = x_coords, y = y_coords, z = z_coords, col = colors[data2])  
# 
# # 添加坐标轴标签和标题（可选）  
# axes3d()  
# title3d(main = "2x2x2 3D Array Visualization")

mice = data.frame(mice)
str(mice)
g = ggplot(data = mice, mapping = aes(x = genotype, y = lifespan, color = sex))
g = g + geom_point(mapping = aes(size = Freq))
g = g + facet_grid(~sex) ## !!! NOTE: Very important!
g

df <- as.data.frame(as.table(data2))  
str(df)

colnames(df) <- c("geneX", "lifespan", "sex", "value")  
df$geneX = as.character(df$geneX)
df$lifespan = as.character(df$lifespan)
df$sex = as.character(df$sex)

# 重命名列  

df
df$geneX[df$geneX == "A"] = "WT"
df$geneX[df$geneX == "B"] = "KO"
df$lifespan[df$lifespan == "A"] = "Alive"
df$lifespan[df$lifespan == "B"] = "Dead"
df$sex[df$sex == "A"] = "Male"
df$sex[df$sex == "B"] = "Female"
df

# 使用ggplot2绘制散点图  
ggplot(df, aes(x = geneX, y = lifespan, color = factor(sex), size = value)) +  
  geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0.1)) +  
  scale_size_continuous(range = c(1, 5)) + # 调整点的大小范围  
  theme_minimal() +  
  labs(title = "Visulization of categorical data of mice",  
       x = "GeneX",  
       y = "Lifespan",  
       color = "Sex",  
       size = "Value")

### Additional exercise

# df2 = spread(df, key = "lifespan", value = "value")
# df2$survial_rate = df2$Alive / (df2$Alive + df2$Dead)
# # df2 = df2[, -which(names(df2) == "Alive" | names(df2) == "Dead")]
# df2

# model = aov(survial_rate ~ geneX * sex, data = df2)
# resid(model)
# summary(model)
# # H0: There is no effect of geneX and sex on survival of mice.
# # HA: At least one of these factors (geneX or sex) influences survival of mice.
# hist(resid(model))
# plot(model, 2)
# shapiro.test(resid(model))
# plot(model, 1)
# TukeyHSD(model)

# Chi - Square检验主要用于比较两个或两个以上样本率（构成比）以及两个分类变量的
# 关联性分析。它属于非参数检验的范畴，适用于配合度检验，即根据样本数据的实际频
# 数推断总体分布与期望分布或理论分布是否有显著差异。卡方检验的根本思想在于比较
# 理论频数和实际频数的吻合程度或拟合优度问题。
# 
# ANOVA（方差分析）则是一种参数检验方法，主要用于比较三组或更多组之间的平均值
# 差异。ANOVA假设各组数据来自正态分布的总体，并且各组之间的方差是相等的（即方
# 差齐性）。通过计算各组之间的方差和组内方差，ANOVA可以确定不同组别之间的差异
# 是否显著。
# 
# 因此，Chi - Square检验和ANOVA的主要区别在于它们的应用对象和假设条件。
# Chi - Square检验主要用于分类变量的关联性分析，而ANOVA则用于连续变量的
# 平均值比较。此外，Chi - Square检验属于非参数检验，对数据的分布没有严格
# 的要求，而ANOVA则是一种参数检验，对数据的正态性和方差齐性有一定的要求。

# Now we want to know if the survival of mice is dependent on geneX and sex 
# (without assuming geneX and sex are independent on each other).

# Why use chi-square test but not anova?
# NOTE: (without assuming geneX and sex are independent on each other) !!!
# H0: The survival of mice is independent on genotype and sex.
# HA: The survival of mice is dependent on genotype and sex.

mouse_data2 <-
  matrix(c(40, 9, 34, 7, 20, 15, 25, 20), nrow = 2, byrow = F)
dimnames(mouse_data2) = list(c("Alive", "Dead"),
                             c("WT_Male", "WT_Female", "KO_Male", "KO_Female"))
chisq.test(mouse_data2)
# when applying chisqu.test on a 2D matrix, the expected value for each cell 
# total_sum * probability_for_row_type * probability_for_column_type 

