# Load the data
library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
setwd("E:\\A大学\\大二下\\ADS2\\W23 4.15-4.19 Supervised and unsupervised learning")
mnist_raw <- read_csv("mnist_train.csv", col_names = FALSE)

head(mnist_raw)
colnames(mnist_raw)
summary(mnist_raw)
str(mnist_raw)

# The first column denotes digit label (0, 1, …, 9) and
# the other 784 columns denote pixel value, between 0 (white) and 255 (black).
# As you could see from the histogram, most values are near 0 or 255, 
# with not a lot in between.

# str(mnist_raw[1, 3])
# str(as.numeric(mnist_raw[1, 3]))
hist(as.matrix(mnist_raw[ , 2: 785]), )

# If you would like to use the suggested transformation,
# you can use the following code
nrow(mnist_raw)

pixels_gathered <- mnist_raw %>%
  head(1000) %>%
  # take only the first 1000 examples,
  # because total 60000 examples are too large for computation
  rename(label = X1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  arrange(instance) %>%
# head(pixels_gathered)
# last(colnames(pixels_gathered))
# head(pixels_gathered)
# head(mnist_raw)
# unique(pixels_gathered[, "pixel"])
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
# pixel 是你想要提取信息的数据框（data frame）或tibble中的列名。
# "pixel" 是原始列的名字，这里与列名 pixel 相同，但也可以是其他任何名字，
# 作为提取操作的引用。
# "(\\d+)" 是一个正则表达式，用于匹配一个或多个数字。这里的 \\d 代表数字，
# + 表示一个或多个。在 R 中，你需要使用两个反斜杠 \\ 来表示一个反斜杠 \，
# 因为在字符串中反斜杠是一个转义字符。
# convert = TRUE 表示在提取后，将结果转换为适当的数据类型（在这里是数字类型）。
  mutate(pixel = pixel - 2, x = pixel %% 28, y = 28 - pixel %/% 28)
# 784 pixels = 28 * 28 把 vector 还原成 matrix
# %% 模运算, %/% 整除运算, 下标变成0再还原好做很多
head(pixels_gathered)

# For example, we can visualize the first 12 instances
# with a couple lines of ggplot2.
theme_set(theme_light())

pixels_gathered %>%
  filter(instance <= 12) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  # 用于创建矩形瓦片图。它通常用于可视化二维数据集，
  # 其中矩形的颜色编码表示数据的值
  facet_wrap(~ instance + label)

# From the legend above, it looks like 0 represents blank space
# (like the edges of the image), and a maximum around 255 represents
# the darkest points of the image. Values in between may represent
# different shades of “gray”.
# How much gray is there in the set of images?

ggplot(pixels_gathered, aes(value)) +
  geom_histogram()

# Most pixels in the dataset are completely white, along with another set
# of pixels that are completely dark, with relatively few in between.
# If we were working with black-and-white photographs
# (like of faces or landscapes), we might have seen a lot more variety.
# This gives us a hint for later feature engineering steps: if we wanted to,
# we could probably replace each pixel with a binary 0 or 1 with very little
# oss of information.

# I’m interested in how much variability there is within each digit label.
# We can find the mean value for each position within each label.

pixel_summary <- pixels_gathered %>%
  group_by(x, y, label) %>%
  summarize(mean_value = mean(value)) %>%
  ungroup()

pixel_summary

# We visualize these average digits as ten separate facets.

pixel_summary %>%
  ggplot(aes(x, y, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "black", mid = "gray",
                       midpoint = 127.5) +
  # 设置gradient时设置了midpoint
  facet_wrap(~ label, nrow = 2) +
  # facet时可以设置图片矩阵的分布nrow
  labs(title = "Average value of each pixel in 10 MNIST digits",
       fill = "Average value") +
  theme_void()

# These averaged images are called centroids.
# We’re treating each image as a 784-dimensional point (28 by 28),
# and then taking the average of all points in each dimension individually.
# One elementary machine learning method, nearest centroid classifier,
# would ask for each image which of these centroids it comes closest to.
# 就是说辨别一个手写的数字是什么的时候，把这个数字和这十个质心图相比较，
# 因为每一个图都代表每一个数字手写的average情况

# Already we have some suspicions about which digits might be easier to separate.
# Distinguishing 0 and 1 looks pretty straightforward: you could pick a few
# pixels at the center (always dark in 1 but not 0), or at the left and right
# edges (often dark in 0 but not 1), and you’d have a pretty great classifier.
# Pairs like 4/9, or 3/8, have a lot more overlap and will be a more challenging
# problem. 上面这段话是在说如何区分0和1这两个书写的数字

# Atypical instances

# So far, this machine learning problem might seem a bit easy:
# we have some very “typical” versions of each digit. But one of the reasons
# classification can be challenging is that some digits will fall widely outside
# the norm. It’s useful to explore atypical cases, since it could help us
# understand why the method fails and help us choose a method and engineer
# features.

# In this case, we could consider the Euclidean distance
# (square root of the sum of squares) of each image to its label’s centroid.

pixels_joined <- pixels_gathered %>%
  inner_join(pixel_summary, by = c("label", "x", "y"))
# 居然R也有类似于MySQL的inner_join

image_distances <- pixels_joined %>%
  group_by(label, instance) %>%
  summarize(euclidean_distance = sqrt(mean((value - mean_value) ^ 2)))

image_distances

# Measured this way, which digits have more variability on average?

ggplot(image_distances, aes(factor(label), euclidean_distance)) +
  geom_boxplot() +
  labs(x = "Digit",
       y = "Euclidean distance to the digit centroid")

# It looks like 1s have especially low distances to their centroid: for
# the most part there’s not a ton of variability in how people draw that
# digit. It looks like the most variability by this measure are in 0s and 2s.
# But every digit has at least a few cases with an unusually large distance
# from their centroid. I wonder what those look like?
#   
# To discover this, we can visualize the six digit instances that had the least
# resemblance to their central digit.

worst_instances <- image_distances %>%
  top_n(6, euclidean_distance) %>%
  mutate(number = rank(-euclidean_distance))

# head(worst_instances, 13)
# 因为上面image_distances没有ungroup()所以相当于是对每一个数字都取了在计算
# 欧几里得距离这个measure方法下偏离程度最大的6个example

pixels_gathered %>%
  inner_join(worst_instances, by = c("label", "instance")) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_gradient2(low = "white", high = "black", mid = "gray", midpoint = 127.5) +
  facet_grid(label ~ number) +
  # 相当于在面上facet可以指定行和列分别按照哪一个factor来facet
  labs(title = "Least typical digits",
       subtitle = "The 6 digits within each label that had the greatest distance to the centroid") +
  theme_void() +
  theme(strip.text = element_blank())

# If you operate on pixels_gathered from now on, you could rm(mnist_raw)
# to clear memory. If you don’t, you may keep only the first 1000 rows of
# mnist_raw for the same purpose.

## Pairwise comparisons of digits

# So far we’ve been examining one digit at a time,
# but our real goal is to distinguish them. For starters, we might like to
# know how easy it is to tell pairs of digits apart.
# To examine this, we could try overlapping pairs of our centroid digits,
# and taking the difference between them. If two centroids have very little
# overlap, this means they’ll probably be easy to distinguish.

# digit_differences <- crossing(compare1 = 0:9, compare2 = 0:9) %>%
#   filter(compare1 != compare2) %>%
#   mutate(negative = compare1, positive = compare2) %>%
#   gather(class, label, positive, negative)
# tail(digit_differences)

pixel_summary

digit_differences <- crossing(compare1 = 0:9, compare2 = 0:9) %>%
  filter(compare1 != compare2) %>%
  mutate(negative = compare1, positive = compare2) %>%
  gather(class, label, positive, negative) %>%
  inner_join(pixel_summary, by = "label") %>%
  select(-label) %>%
  # select all the columns except "label"
  spread(class, mean_value)

digit_differences
# 这个dataframe的意义就在于，当compare一个digit pair (compare1, compare2)时
# positive代表这个(x,y)坐标位置的像素点的mean_value是是compare2这个digit的
# negative代表这个(x,y)坐标位置的像素点的mean_value是是compare1这个digit的

ggplot(digit_differences, aes(x, y, fill = positive - negative)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = .5) +
  facet_grid(compare2 ~ compare1) +
  theme_void() +
  labs(title = "Pixels that distinguish pairs of MNIST images",
       subtitle = "Red means the pixel is darker for that row's digit,
       and blue means the pixel is darker for that column's digit.")
# 在这个图中，极红和极蓝的点都可以区分digit pair，如果像素点差异不大，那么颜色、
# 接近白色，也可以从图虫看出，蓝色的图案是compare2这个digit的，而红色的图案是
# compare1这个digit的

# Pairs with very red or very blue regions will be easy to classify,
# since they describe features that divide the datasets neatly.
# This confirms our suspicion about 0/1 being easy to classify: it has
# substantial regions than are deeply red or blue.
# 
# Comparisons that are largely white may be more difficult.
# We can see 4/9 looks pretty challenging, which makes sense
# (a handwritten 4 and 9 really differ only by a small region at the top).
# 7/9 shows a similar challenge.



rm(mnist_raw)
# clear the memory for mnist_raw

# In the transformed dataset, label is digit label (0..9),
# instance is example number (1..1000),
# pixel is pixel number (0..783),
# equal to column numbers in the original dataset minus 2,
# value is pixel value (0..255) and
# x (0..27) and y (1..28) are pixel coordinates that can help you compute
# statistics over pixels in the same column or row.
# Note that unlike in the original dataset,
# observations are no longer examples, but example-pixel pairs.
# 意思是 original dataset mnist_raw中，每一行代表一个example,把pixel matrix
# 当成 vector 储存了，但是现在我们把 vector 中每一个值拿出来 gather() 成每个
# example有784个pixel pairs了

# Based on the features you selected, create and compute a features dataframe,
# where rows are examples and variables/columns are different features
# (e.g. 56 of them).

pixels_gathered$x = as.integer(pixels_gathered$x)
pixels_gathered$y = as.integer(pixels_gathered$y)
pixels_gathered

pixels_gathered$x = as.factor(pixels_gathered$x)
pixels_gathered$y = as.factor(pixels_gathered$y)
pixels_gathered

# feature 1: Average value on each row and each column (28 + 28 = 56 features)
calc_avg = function(df){
  df_x = df %>%
    group_by(x) %>%
    summarize(avg_x = mean(value)) %>%
    spread(key = x, value = avg_x)
  colnames(df_x) = paste0("avgx", colnames(df_x))
  # print(df_x)
  df_y = df %>%
    group_by(y) %>%
    summarize(avg_y = mean(value)) %>%
    spread(key = y, value = avg_y)
  colnames(df_y) = paste0("avgy", colnames(df_y))
  # print(df_y)
  cbind(df_x, df_y)
}
#calc_avg(pixels_gathered[pixels_gathered$instance == 1, ])

trans = function(df){
  count = 0
  for(i in 2:length(df))
    if((df[i-1]<128 && df[i]>127) || (df[i-1]>127 && df[i]<128))
      count = count + 1
  return (count)
}

calc_trans = function(df){
  df_x = df %>%
    group_by(x) %>%
    summarize(trans = trans(.$value)) %>%
    spread(key = x, value = trans)
  colnames(df_x) = paste0("transx", colnames(df_x))
  df_y = df %>%
    group_by(y) %>%
    summarize(trans = trans(.$value)) %>%
    spread(key = y, value = trans)
  colnames(df_y) = paste0("transy", colnames(df_y))
  cbind(df_x, df_y)
}

feature = pixels_gathered %>%
  group_by(instance, label) %>%
  group_split() %>%
  map(~cbind(instance = .$instance[1],
             label = .$label[1],
             calc_avg(.),
             calc_trans(.))) %>%
  bind_rows()
  # mutate(group_type = if_else(is.na(avg_y), "x", "y")) %>%
  # select(instance, label, group_type, contains("avg")) 

feature

write.csv(feature, file = "features.csv", row.names = F)
