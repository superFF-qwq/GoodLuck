rm(list = ls())
setwd("E:\\A大学\\大二上\\ADS2\\Week 7 Getting and cleaning data")
wnv = read.csv("Week_7_WNV_mosquito_test_results.csv")
library(tidyverse)
#rename(file,YEARS = SEASON.YEAR)

#1.1. Explore our data
#data structure
class(wnv)

head(wnv)
class(wnv$TEST.DATE)

# Let's try to make it a real date/time type
wnv$TEST.DATE = as.POSIXct(wnv$TEST.DATE, format = "%m/%d/%Y %H:%M:%S",
                           tz = "America/Chicago")
#class(wmv$TEST.DATE)
#head(wmv$TEST.DATE)

#Let's try to convert timezone of daytime type data
dat1 = wnv$TEST.DATE[1]
dat1
attributes(dat1)$tzone
attributes(dat1)$tzone = "America/Los_Angeles"
dat1

#Let's try to separate LOCATION column into LATITUDE and LONGITUDE
head(wnv$LOCATION, 1)
#gsub(pattern, replacement, x)
head(wnv, 1)
wnv$LOCATION = gsub("[()]", "", wnv$LOCATION, perl = T)
# gsub()函数用于替换字符串中的指定模式（正则表达式）为新的字符串。
# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
# useBytes = FALSE)
# pattern: 要匹配和替换的正则表达式模式。
# replacement: 替换模式。可以是一个字符串，也可以是一个函数。
# x: 要在其中进行替换操作的输入字符串或字符向量。
# ignore.case: 一个逻辑值，指定是否忽略模式匹配时的大小写，默认为FALSE。
# perl: 一个逻辑值，指定是否使用Perl风格的正则表达式，默认为FALSE。
# fixed: 一个逻辑值，指定是否将pattern视为普通字符串而不是正则表达式，默认为FALSE。
# useBytes: 一个逻辑值，指定是否按字节而不是字符处理输入字符串，默认为FALSE。
wnv = separate(
  wnv,
  LOCATION,
  into = c("LATITUDE", "LONGITUDE"),
  sep = ",",
  remove = F,
  fill = "left",
  convert = T
)
## separate() convert a long format into a wide format
## separate(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, convert = FALSE)
## data: 需要拆分的数据框或数据集
## col: 需要进行拆分操作的列名或列索引
## into: 指定新的列名，可以是字符向量或一个整数，表示需要创建的新列数量
## sep: 用于指定拆分每个元素的分隔符。默认为非字母数字字符
## remove: 一个逻辑值，用于指定是否删除原始列。默认为TRUE，表示删除
## convert: 一个逻辑值，用于指定是否将拆分出的新列转换为数值类型，默认为FALSE
head(wnv, 1)
summary(wnv)

#### There other useful ways to do string manipulations ######
library (stringr) # for example.
#### explore other functions
# str_sub(), str_replace() , str_split()

#1.2 Screen and diagnosis.

# Just for fun / practice purpose, let's try the functions gather and spread.
# In order not to mess up the original dataset, make a new transformed dataset
# first, let's load the library for this.

#library(tidy)
wnv2 = gather(wnv)
# gather函数的功能是将宽数据准换为长数据
# 函数形式：gather(data,key,value,...,na.rm=F)
# key：将原数据框中的所有列赋给一个新变量key
# value：将原数据框中的所有值赋给一个新变量value
# …：指定变量聚到同一列中，可通过变量在数据集中的列数，也可以使用"-"排除特定的变量
# na.rm：是否删除缺失值
wnv2
str(wnv2) # This data as it is too messy. We will remove it.
rm(wnv2)

### Are the variable names informative?
table(wnv$SEASON.YEAR)
# table() count the amount of every factor
# Looks that there is no season in it.
# Missing values? Duplicated rows? Anything to change?
anyNA(wnv) # there are, but where?
which(!complete.cases(wnv)) # quite a lot.
# Let's have a look at them.
wnv[17,] # We saw NA values and missing space.

length(wnv[wnv$LOCATION == "", "LOCATION"])
length(rownames(wnv[is.na(wnv$LATITUDE),]))
length(wnv[is.na(wnv$LATITUDE), "LATITUDE"])
# 4416 rows with these missing values. It is quite a lot.
# Any duplicated values?
which(duplicated(wnv))
# None.
# Let's check if the datatypes are ok.
str(wnv) # What do you think? Shall we change any of them?

# Strange patterns?
### We need to plot to check if there are any strange patterns.
library(ggplot2)
library(viridis) # Color-blind friendly color

# Let's check if the relationships between number of mosiquitoes
# and different factors.
# year?
ggplot(data = wnv, aes(x = as.factor(SEASON.YEAR), y = NUMBER.OF.MOSQUITOES)) +
  geom_boxplot()
# Latitute?
ggplot(data = wnv, aes(x = LATITUDE, y = NUMBER.OF.MOSQUITOES)) +
  geom_point(alpha = 0.3, size = 1, aes(col = as.factor(SEASON.YEAR))) +
  scale_color_viridis(discrete = T) + geom_smooth()
# alpha 透明度
# Longitude?
ggplot(data = wnv, aes(x = LONGITUDE, y = NUMBER.OF.MOSQUITOES)) +
  geom_point(alpha = 0.3, size = 1, aes(col = as.factor(SEASON.YEAR))) +
  scale_color_viridis(discrete = T) + geom_smooth()
# Species & TRAP
ggplot(data = wnv, aes(
  x = as.factor(TRAP_TYPE),
  y = NUMBER.OF.MOSQUITOES,
  col = SPECIES
)) +
  geom_boxplot() +
  scale_color_viridis(discrete = T)

#### Please explore the way you want.
## If you want to save any figures,
# You could click "Plots" pane & "Export"
# Or, use the function  ggsave() to export file as you wish.
# Play with the parameters if you want.

# One outlier. Which is it?
wnv[wnv$NUMBER.OF.MOSQUITOES > 70,]

### Treatment.
#1. Variable name for SEASON.YEAR should be changed to YEAR
names(wnv)[1] = "YEAR"
#2. Shall we remove the NA values? If you want to remove, we could:
wnv = wnv[!is.na(wnv$LONGITUDE),]

#3. Shall we remove the outlier?
#### If you think we should, but only with SOLID REASON.

wnv_plot = ggplot(data = drop_na(wnv)) + theme_classic()
wnv_plot + geom_boxplot(mapping = aes(x = round(LONGITUDE, 3),
                           y = NUMBER.OF.MOSQUITOES,
                           group = round(LONGITUDE, 3)),
                           #group = cut_width(LONGITUDE, 0.1),
                           #color = round(LONGITUDE, 3)),
             position = "identity",
             outlier.shape = NA) +
  scale_x_continuous() #+ scale_color_viridis(discrete=F)

# 绘制箱线图，如果有分组(即有多个箱线图)的话
# x轴对应的变量最好是分类变量或因子变量(如cut)而不是数值变量(carat)；
# 如果x轴对应的变量确实要是连续型的数值变量(如carat)，
# 那么就需要加入分组group的设置，否则x轴上如何画出对应的若干个箱线图呢，
# 总不能每个数值点画一个箱线图吧？
# 分组group的功能相当于“分箱”,
# 即通过cut_width设置等宽度分箱或用cut_number设置固定数量分箱。
# geom_boxplot(aes(group = cut_width(carat, 0.25))

#### Now, if you want, you may want to save the file as a R data or export as a file.
save(wnv, file = 'wnv_modified_2023.RData')
# if you want to export as a table,
write.table(wnv,
            file = "wnv_modified_2023.txt",
            sep = '\t',
            quote = F)

# Make sure that you document all the changes that you implemented
# in your electronic notebook.
# Variable names changed.
# Especially, which data point are removed & Why.
