setwd("E:\\A大学\\大二上\\ADS2\\Week 7 Getting and cleaning data")
#####
####### Practice 2
########
# HINT: ELISA kit.
# Op stands for "optical density" 光密度，吸光度，反应目的蛋白含量
# First, let's clean up the environment.
#rm(list = ls())
#dev.off()

# Load the datatable. This time, it is a tab separate txt file.
# Therefore we used read.table rather than read.csv.
pgp3_og = read.table("Week_7_Tests_PGP3.txt", sep = '\t', header = T)

# Have a look at the data and get a feeling.
head(pgp3_og)
summary(pgp3_og)
str(pgp3_og)
table(pgp3_og$measured)
table(pgp3_og$SampleID)
which(duplicated(pgp3_og)) # ok there are duplicated rows.
idx1 = which(duplicated(pgp3_og))
idx2 = which(duplicated(pgp3_og, fromLast = T))
# fromLast = T look for duplicated from right to left,
# the first one will be thought as FALSE
# duplicated() returns a list of TRUE/FALSE
# which() returns the index
idx1
idx2

# Since it only gives one set of index duplicated rather than both.
# Let's check whether duplication is true.
pgp3_og[c(idx1, idx2),]
# it is safe to keep only one copy,so then we can
# remove the duplicated in the following

### duplicated() will only give you the duplicated rows,
### BUT not the original rows, so we need fromlast = T
### to the next line to get the originals

# We already saw that some data seems to be missing.
anyNA(pgp3_og)
# There are missing values.
pgp3_og[which(!complete.cases(pgp3_og)),]
pgp3_og[!complete.cases(pgp3_og),]
length(rownames(pgp3_og[complete.cases(pgp3_og),]))
# 2219, less than number of rows that is 2324.

####
#### Treatment.
#1. it is safe to keep only one set of duplicated rows.
pgp3 = pgp3_og[-idx1,]# remove one set of the duplicated rows.
# Let's check if all the duplicated rows are removed.
which(duplicated(pgp3)) # ok.

#2. transform the long format to a wide format.
pgp3 = spread (pgp3, measured, value)
# spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE)
# key：指定转换的某列，其观测值作为转换后的列名
# value：其他列的观测值分散到相对应的各个单元
# fill：对于缺失值，可将fill的值赋值给被转型后的缺失值
head(pgp3)
summary(pgp3)
str(pgp3)
table(pgp3$sex)  # hard to understand
table(as.character(pgp3$sex))
# 1 and 2. From the source of data we know 1 is male and 2 is female.

#3. Data types
#3.1 The SampleID should be factors, NOT integers.
#3.2 The elisa od values shouldn't be factors. Change them to numeric values.
#3.3 The sex variable is not easily interpretable. change them to M and F.

#covert SampleID, age.F, sex to readable factor
pgp3$SampleID = as.character(pgp3$SampleID)
pgp3$elisa.od = as.numeric(as.character(pgp3$elisa.od))
# Here, we change the factor to character first.
# What would happen if you don't convert to character first?
# 一个常见问题是把factor转换成numeric。
# 比如表示年份的变量，被编码为factor类型的年份变量，需要转换成numeric用于计算。
# 要点是，直接对factor变量使用as.numeric没有意义和不可控，会导致系统采用隐式转换。
pgp3$elisa.pre.od = as.numeric(as.character(pgp3$elisa.pre.od))

pgp3$sex = gsub("1", "M", as.character(pgp3$sex))
pgp3$sex = gsub("2", "F", as.character(pgp3$sex))
# revision: similar to w7-p1, gsub(pattern, replacement, string)
pgp3$sex = as.factor(pgp3$sex)
summary(pgp3)

#4. Missing values.
# Shall we remove them? decided to drop them out.
length(rownames(pgp3[!complete.cases(pgp3),]))
# 103, as in the summary for sex values missing.
pgp3 = pgp3[complete.cases(pgp3),]
anyNA(pgp3)

#5. Any strange patterns?
ggplot(data = pgp3) + geom_boxplot(aes(x = sex, y = elisa.od))
ggplot(data = pgp3) + geom_boxplot(aes(x = age.f, y = elisa.pre.od))
# What is going on? The x-axis label of the first group is missing.
# These are not dropped since it is not recorded as NA values.
# These should be removed,
# considering we are interested in the relationship between age and elisa od.
head(pgp3[pgp3$age.f == '',])
pgp3 = pgp3[pgp3$age.f != '', ]

ggplot(data = pgp3) + geom_boxplot(aes(x = age.f, y = elisa.pre.od))
ggplot(data = pgp3) + geom_boxplot(aes(x = age.f, y = elisa.od))

#6.  We want to reshape the dataframe so that we could compare ELISA od
# at two time points at each age group.
# For this purpose of getting a long format, reshape the dataframe again.

head(pgp3)

pgp3 = gather(
  pgp3,
  key = "time.point",
  value = "ELISA.od",
  elisa.od:elisa.pre.od,
  factor_key = T
)
# gather(data=,key=,value=,...,na.rm=,convert=,factor_key=)
# key：创建一个新的列名，原数据的旧列名成为新列名的观测值
# value：再创建一个新的列名，原数据的所有旧列名的观测值成为新列名的观测值
# ...：按照实际需要自行指定需要转换的列
# na.rm：逻辑值，是否删除缺失值
# convert：逻辑值，在key列是否进行数据类型转换
# factor_key:逻辑值，若是F，则key自动转换为字符串，反之则是因子，level不变
# gather之后会把新生成的列放在最后几列

head(pgp3)
summary(pgp3)

ggplot(data = pgp3, aes(age.f, ELISA.od, color = time.point)) + geom_boxplot()
ggplot(data = pgp3, aes(x = age.f,
                        y = ELISA.od,
                        fill = time.point)) + # fill指填充颜色
  geom_boxplot(color = "black") + # color指边框颜色
  theme_classic() + #经典外观的主题，带有 X轴和 Y 轴线，没有网格线
  scale_fill_manual(labels = c("elisa.pre.od", "elisa.od"), #设置颜色对应标签
                    name = "Measurement", #图例名称
                    values = c("#ADD8E6", "#FF6347"), #D Shytikov的配色方案
                    breaks = c("elisa.pre.od", "elisa.od")) +
                    # breaks 指定不同变量，来对应不同颜色
  #scale_fill_manual 手动设置填充颜色
  theme(legend.position = "right") + #其实不用加，默认就是right
  guides(color = guide_legend(override.aes = list(size = 3) ) )

ggsave(file = 'ELISA_od.pdf',
       width = 7,
       height = 5)
save(pgp3, file = 'pgp3_2023_modified.RData')

# Documentation
# Xianghua changed the data
# 1) from long to wide
# 2) changed datatypes
# 3) changed the format of sex value
# 4) removed missing values that occur in age range and sex
# NOTE: anyNA is not enough to remove all the missing values.
# Some missing is not considered as NA.
# Plot the data to check whether there is any NA.
# Use other strategies to remove NA.
# 5) Reshaped the dataframe again by gathering the two od values into
#    a key called time.point.  