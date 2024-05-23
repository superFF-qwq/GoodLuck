# practical 9: Data visualization - adapted version

setwd("E:\\A大学\\大二上\\ADS2\\Week 9 Data visualization")
library(tidyverse)

# 1. Overplotting. 

data = diamonds
ggplot(data, aes(x = carat, y = price, color = cut)) +
  geom_point()
# geom_point 添加散点图
# ggplot(data, aes(x = carat, y = price, color = cut)) +
#   geom_point(stat = "identity", mapping = aes(color = cut))
# 实践证明geom_point() 里面不加东西是一样的，因为默认stat = "identity"
# aes(group = cut) 好像达到的是相同的效果
# aes是'aesthetic'的缩写,意为'美学',通过aes函数可以对图表中的各种要素进行美学设定
# aes()中的group参数为不同分组，color参数配置不同分组的颜色

ggplot(data, aes(x = carat, y = price, color = cut)) +
  geom_point(position = "identity", size = 0.1, alpha = 1/5) +
# position = "identity" 使用原始数据绘图
# position = "jitter" 向数值向量添加少量噪声, 抖动数据
# 将数据点在x轴方向上稍微分散开，以避免重叠
# 它通常用于更好地可视化重叠值，例如整数型变量。这有助于掌握观察密度高的地方。
# 如果某些值已抖动，即使很明显，最好在图形图例中提及。如 Jittered x-axis.
# 用法也可以是 jitter(vector)
  guides(color = guide_legend(override.aes = list(size = 6, alpha = 0.5)))
# 用 guides() override重新自定义图例， 原图例中size太小，
# 因为同步了figure中的size，shape，alpha

# You can see that the dots(points) are overlaid.

## 1.1 To make it better. (size: size and transparency: alpha)
ggplot(data, aes(x = carat, y = price, color = cut)) +
  geom_point(size = 0.1, alpha = 1/5)

## 1.2 change the shape of dots.
ggplot(data, aes(x = carat, y = price, color = cut)) +
  geom_point(shape = 18) # shape = 18 is diamond

# 2. Rewrite the code.

# In the second of lecture slides 
# (choosing different geom_XXXX and stat_XXXX functions), we use the stat_bin
# function to generate two plots. Please try to rewrite the code and
# use geom_XXX and stat_XXX to generate the same plots.

ggplot(data, aes(x = carat)) + 
  geom_area(stat = "bin", binwidth = 0.1) +
  # geom_area() 绘制面积图，可将定量的数量相对于其他数量的变化可视化
  # stat = "identity"，该语句表示直接使用原始数据进行绘图。
  # stat = "bin" 直方图的绘制原理是先将连续变量分段、统计频数，
  # 然后再绘制成“柱状图”，这就是分箱统计变换。
  scale_y_continuous(breaks = seq(0, 10000, 2000), limits = c(0, 10500)) +
  scale_x_continuous(breaks = seq(0, 3, 0.5), limits = c(0,3))
  # 自定义连续型刻度Y轴
  # seq(from,to,by) 生成一组数字

ggplot(data, aes(x = carat)) + 
  geom_point(aes(size = ..density..), stat = "bin", binwidth = 0.1) +
  # size = ..density.. 用size代表density
  # 也可以aes(y = ..density..) 用y轴表示density
  # fill = ..density.. 颜色深的地方密度大
  scale_x_continuous(breaks = seq(0, 3, 0.5), limits = c(0, 3))

gg1 =
  ggplot(data = diamonds,
         mapping = aes(x = carat, y = price, color = cut))
g2 = gg1 + stat_identity(
  mapping = aes(color = cut),
  size = 0.6,
  geom = "point",
  shape = 3, # shape = 3 是加号+
  position = "jitter"
)
g2

# 3. Build plots layer by layer 指一层一层往图上加东西

## 3.1 Plot the following boxplot from the dataset “diamonds”.

base = ggplot(data, aes(x = clarity, y = carat, color = cut)) +
  geom_boxplot(outlier.size = 0.8)
# 改变outlier.size也会相应的有调整non-outlier数据点size
base

## 3.2  generate another layer of linear fitting using geom_smooth, 
##      use method lm. Save the geom_smooth to a new object sm.

sm = geom_smooth(aes(group = cut, x = as.integer(clarity)), method = "lm", se = F)
# NOTE how the x-value was changed 但是这好像是默认的，不加好像没关系
# geom_smooth() 可用于生成光滑曲线或回归线
# mapping = aes() mapping默认的，可以不加
# 默认使用局部加权回归（LOESS）方法拟合数据
# method = "loess"或"lm"，默认是"auto"，自动判断使用什么回归方法。
# formula = y ~ x或y ~ poly(x, degree) 用于指定光滑曲线的形式
# degree是多项式的最高次数。
# se = T or F 控制是否在光滑曲线两侧显示置信区间。
# level = 0 ~ 1 设置置信区间的置信水平。默认值为0.95
# span 参数制局部加权回归方法的平滑度。数值越大，平滑度越大，曲线越平滑。
# 默认值为0.75。
# color和linetype参数用于控制光滑曲线的颜色和线型。
# color: "red", "blue", "green", ...
# linetype: "dashed", "dotted", "solid", "blank"

ggplot(data, aes(x = clarity, y = carat)) +
  geom_boxplot(outlier.size = 0.8) +
  geom_smooth(aes(x = as.integer(clarity)), method = "lm", size = 0.7)
# geom_smooth 中 size 设置了线的粗细， 一定要指定 method = "lm"

ggplot(data, aes(x = clarity, y = carat, color = cut)) +
  geom_boxplot() +
  sm

## 3.3 Faceting the plot by cut, color and cut~color.
# facet 分面（分图），即用原始数据按照cut, color和cut-color分成几个小图

Facet1 = facet_wrap(~cut)
Facet2 = facet_wrap(~color)
# facet_wrap() 用于在一个图形绘制中创建一个包含多个子图的网格布局。
# 它可以帮助我们将数据的不同方面可视化，并比较它们之间的差异和联系。
# facet_wrap() 可以将一个变量拆分为多个子图
# facet_wrap() 最基本的用法，就是按某个变量分组展示数据。
# facet_wrap(~变量名, nrow=行数, ncol=列数)。
# 其中~变量名表示要根据哪个变量进行分面，nrow和ncol分别表示行数和列数，
# 即要创建的子图数量。

Facet3 = facet_grid(cut~color)
# facet_grid的两个参数，分别代表不同行和不同列的变量名称。
# facet_grid(cut ~ color)，它代表了首先将钻石切面按照行的方式分组，
# 然后将钻石颜色按照列的方式分组

base + Facet1
base + Facet2
base + Facet3

## 3.4 Add another layer to add a title to the plot using labs

title = labs(title = "Carat vs clarity")
base + ggtitle("Carat vs clarity") +
        theme(
          plot.title = element_text(hjust = 0.5),
          # make the label in the middle
          axis.text.x = element_text(angle = 45),
          # make the x-axis scale 刻度 rotate 45 degrees
          legend.position = "bottom"
          # change the location of the graph legend
          # legend.position = "none" 隐藏图例
        )

# theme()
# theme(主题.部件=element_类型())
# 主题: plot, axis,legend, panel, facet
# 部件: title(名字，坐标轴名字),line(线，坐标轴的xy轴),
# text(标签，坐标轴刻度的数字),ticks(坐标轴刻度的小线条), background(背景)等
# 类型: rect(所有矩形区域属性),line(所有线属性),text(所有文本相关属性)，
# title(所有标题属性)
# 说明: 部件要和类型一致。比如，部件为title，text等文字相关的元素，
# 那么类型处就为text (具体见本week中的theme_cheatsheet)。

## 3.5 Change the color

color = scale_color_brewer(palette = "Purples")
# library("RColorBrewer")
# 使用RColorBrewer包上色：scale_color_brewer(palette = "调色板名称")：
# palette = "Dark2" 真的好丑，但是据说是顶刊常用？
# palette = 3 是 Dmytro Shytikov 选的颜色，挺好看的
# display.brewer.all(type = "all") 展示所有的调色板

Test = base + sm + title + color
Test
ggsave("Test2.png",Test)

## 3.6 Save the plot
save = ggsave("Test.png")

# 4. Scale the y axis

base = ggplot(data, aes(x = clarity, y = carat, color = cut))

## 4.1 Transform the y-axis scale to log10(carat)
## Starting from the plot in 3.2
## transform the y-axis scale to log10 using scale_y_continuous
## change the y-axis label to include the unit using ylab

g4.1 = base + geom_boxplot() + sm + scale_y_continuous(trans = "log10") +
       ylab("log10(carat)")
g4.1

## 4.2 Redo the boxplot with log10(y).

# redo the boxplot in 3.1 by changing the y aesthetics to log10(carat).
# Compare the y-axis here with (use plot_grid to compare) the one in 3.1.
# Change the y-axis label to include the unit.

g4.2.1 = base + # g中x = clarity已经写过了，下面可以不写，再写的话相当于覆写override
  geom_boxplot(mapping = aes(x = clarity, y = carat)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Original plot")
g4.2.1

g4.2.2 = base +
  geom_boxplot(mapping = aes(y = log10(carat))) +
  # 这里相当于把数据点的y值改掉了，但是没有相应调整y scale
  labs(title = "Change the aesthetic",
       caption = paste0("Figure 4.2.2 Change the y value of the point,",
                        "but do not change the y scale.",
                        sep="")
       ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 10,
                                    hjust = 0.5)
        )
g4.2.2

g4.2.2 = base +
  geom_boxplot(mapping = aes(y = log10(carat))) +
  labs(title = "Change the aesthetic")

### NOTE: In particular, the values may change,
###       BUT the axis may require additional attention.

g4.2.3 = g4.2.1 +
  scale_y_continuous(trans = "log10") +
  # 这里配套把y scale重新调整了一下
  ggtitle("Change the Y-scale") ## override the title
g4.2.3

library(cowplot)
# This package allows you locating several `ggplot` objects one by another.
plot_grid(g4.2.1, g4.2.2, g4.2.3, NULL,
          nrow = 2,
          rel_widths = c(0.75, 1),
          # relative columns widths: grid中column width的相对大小
          byrow = T) # default: byrow = T

## 4.3 Add a layer of linear 

# 4.3.1 Add a layer of linear fitting to the plot in 4.2 by + sm from point 3.2.
g4.3.1 = g4.2.3 + sm + theme(legend.position = "none") + ggtitle("")
g4.3.1
# There is a big problem, because the y-axis is transferred into log(y),
# but the linear fitting sm did not change.
# fix the problem by change the aesthetics in sm.

g4.3.2 = g4.2.2 + geom_smooth(
  mapping = aes(x = as.integer(clarity),
                y = log10(carat)), # change the aesthetics
  method = "lm",
  se = F,
  size = 0.7) +
  ggtitle("")
g4.3.2

plot_grid(g4.3.1, g4.3.2,
          rel_widths = c(0.75, 1),
          labels = c("Just apply `sm`", "Fix aesthetics in `sm`"))
# 现在我明白为什么 rel_widths = c(0.75, 1) 了，因为右边的这个图带legend，
# 所以为了让两个plot大小比例看起来更好，让右边的plot更大一点

## 4.4 change the range of y-axis in the plot 4.1. set the limits to 0.3 to 3.0 using scale_y_continuous.

g4.4 = g4.1 + ylim(c(0.3, 1))
g4.4 = g4.1 + scale_y_continuous(limits = c(0.3, 1.0))
g4.4

# 5. Jitter plot and scales.

## Suppose, you want to show the relationship of the cut quality, clarity, mass, 
## and price of the diamond on the same plot.

## 5.1 Make the layer
## Sample out 100 cases from diamonds dataset. Generate a plot
## with a layer of boxplot and a layer of jitter plot using geom_jitter.

sample = data[sample(1:nrow(data), 100), ]
## sample(x, size, replace = FALSE, prob = NULL)
## default: replace = F 无放回抽样
g5 = ggplot(data = sample,
            mapping = aes(x = clarity, y = carat)
            )
g5.1 = g5 + geom_boxplot(outlier.size = 0.8) +
  geom_jitter(width = 0.1,
              mapping = aes(size = carat, color = price))
# geom_jitter() 默认 width = 0.4 即在x轴上抖动程度为80%，
# 还有一个非常大的作用就是在boxplot上把点显示出来了
g5.1

## 5.2 reset the color scale
## Reset the color scale to only color the diamonds
## in the price range(10000, 15000) using scale_color_gradient.

g5.2 = g5.1 +
  scale_color_gradient(low = "red", high = "green", limit = c(10000, 15000))
g5.2 = g5.1 +
  scale_color_gradient(limit = c(10000, 15000))
# g5.1中的color就是按照price分类
g5.2 

## 5.3 reset the size scale
## Reset the size scale to only show the diamonds in the carat range(1.2, 2)
## using scale_size_continous.

g5.3 = g5.2 +
  scale_size_continuous(limit = c(1.2, 2))
# g5.2中的size就是按照carot分类
g5.3
plot_grid(g5.2, g5.3,
  ncol = 2, nrow = 1,
  labels = c("scale_color_gradient", "scale_size_continous"),
  hjust = -0.2,
  vjust = 1)
## plot_grid()
## hjust: label horizontally adjust
## vjust: label vertically adjust

# 6. Position

# Reverse the order of the factor.
data6 = data
new_level = rev(attributes(data6$cut)$levels)
data6$cut = factor(data5$cut, levels = new_level)

# In the lecture, we discussed different position adjustments.
# Generate the plots that can describe the number of different categories
# of diamonds according to their cut and clarity
#Use the position argument in geom_bar.

g6 = ggplot(data = data6,
            mapping = aes(x = clarity, group = cut))
g6.1 = g6 +
  geom_bar(mapping = aes(fill = cut),
           position = "stack") +
  ggtitle("Stacking") +
  scale_y_continuous(limits = c(0,14000), breaks = seq(0,12000,2000)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    legend.position = "n", # delete the legend
    aspect.ratio = 7 / 7 # aspect ratio of the panel 面板的宽高比
  )
g6.1

g6.2 = g6 +
  geom_bar(mapping = aes(fill = cut),
           position = "fill") +
  ggtitle("Filling") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    legend.position = "right",
    aspect.ratio = 7 / 5
  )
  # axis.text随便设置不会改变任何事情
  # axis.text.x和axis.text.y会改变x,y轴刻度的文本大小
g6.2

g6.3 = g6 +
  geom_bar(mapping = aes(fill = cut),
           position = "dodge") +
  ggtitle("Positioning near") +
  scale_y_continuous(limits = c(0,5200), breaks = seq(0,5000,1000)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 10),
    legend.position = "n",
    aspect.ratio = 7 / 7
  )
g6.3

plot_grid(g6.1, g6.2, g6.3, NULL,
          ncol = 2,
          rel_widths = c(0.75, 1.25))
plot_grid(plotlist = list(g6.1, g6.2, g6.3, NULL),
          ncol = 2,
          rel_widths = c(0.75, 1.25))

## geom_bar(mapping = aes(x = , fill = ))
## 不能指定y轴，因为y是count(数量or频率)
## position = "stacking" 数量堆叠
## position = "filling" 频率
## position = "dodge" 按照fill分组显示数量

### Documentation
# 1. add the plot element layer by layer
# 如果a = ggtitle() + theme(), plot + a这样会出错
# 系统估计不能正确处理a，因为感觉两个不同类型的东西加在一块儿了
# 2. ggtitle("") 或者 labs(title = "") 后者一定要写 title = 
# 3. mapping = aes(y = log10(carot)) 一定要配套把y scale也修正过来
#    scale_y_continuous(trans = "log10")
# 4. add figure caption, we use labs(caption = "")
#    then adjust figure caption, we use theme(plot.caption = element_text())
