library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(nnet)
setwd("E:\\A大学\\大二下\\ADS2\\W24 4.22-4.26 Neural networks and support vector machines for classification and regression")

data0 = read.csv("features.csv")

#head(data)

data = data[, -1]

data = data0 %>%
  group_by(label) %>%
  group_split() %>%
  map(~{
    apply(., MARGIN = 2, FUN = mean)
  }) %>%
  bind_rows()
# head(data)
# ncol = length(names(data))
# ncol
# head(data)
head(data)

nfeature = ncol - 1

# data

data2 = data %>%
  filter(label == 0 | label == 1) %>%
  gather(feature, value, -label) %>%
  mutate(feature = rep(1:nfeature, each = 2)) %>%
  filter(feature <= 56)
data2$label = as.factor(as.integer(data2$label))
# data2
# nrow(data2)
ggplot(data2, aes(x = feature, y = value, group = label)) +
  geom_line(aes(color = label))

plot(x = 1:56, y = data2$value[data2$label == 0])
lines(x = 1:56, y = data2$value[data2$label == 1])

features = data0[, 2:58]

for(i in 2:57){
  maxi = max(features[,i])
  if(maxi > 1e-3)
    features[,i] = features[,i] / maxi
}

rows <- sample(1:1000, 700)

train_labels <- features[rows, 1]
valid_labels <- features[-rows, 1]
train_data <- features[rows, -1]
valid_data <- features[-rows, -1]

head(train_labels)

train_labels_matrix = class.ind(train_labels)

head(train_labels_matrix)

## Now we are ready to train our neural network.

nn = nnet(train_data, train_labels_matrix, size = 12, softmax = TRUE)

# It will train a network with 4 hidden units (in one layer),
# performing 100 iterations by default.
# It indicates the number of weights (parameters) and
# development of errors with training.

# To use your train model for classifying data, use the predict command.

pred_train = predict(nn, train_data, type="class")
pred_valid = predict(nn, valid_data, type="class")

# will compute predicted classes for training and validation data

# You can calculate your classification accuracy by using 

mean(pred_train == train_labels)
mean(pred_valid == valid_labels)

# for training and validation data.