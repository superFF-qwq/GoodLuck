setwd("E:\\A大学\\大二下\\ADS2\\W23 4.15-4.19 Supervised and unsupervised learning")

library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)

features = read.csv("features.csv")

head(features)

range(which(grepl("transx", colnames(features))))
range(which(grepl("transy", colnames(features))))

rg = range(which(grepl("trans", colnames(features))))
rgl = rg[1]
rgr = rg[2]

# transx: 50-86
# transy: 87-114

range(features$label)

as.matrix(t(lapply(features[, rgl: rgr], FUN = mean)))

df = features %>%
  group_by(label) %>%
  group_split() %>%
  map(~{
    return (cbind(label = .$label[1],
                  as.matrix(t(lapply(.[, rgl: rgr], mean)))))
  }) %>%
  bind_rows()
# df
colnames(df)
colnames(df) = paste0("avg_", colnames(df))
# colnames(df)
rownames(df)
colnames(df)
df = df %>%
  gather(key = "measurement", value = "value", -label)
df

ggplot(df) +
  
  facet_wrap(~label)

