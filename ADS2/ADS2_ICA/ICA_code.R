library(dplyr)

setwd("E:\\A大学\\大二下\\ADS2")

data = read.csv("substance_use.csv")

data = data %>%
  select(c(measure, location,	sex, age,	cause))
# head(data[duplicated(data),])
# head(data[duplicated(data, fromLast = T),])
# data2 = unique(data)
# nrow(data)
# nrow(data2)
row1 = data[which(duplicated(data))[1],]
row1
index1 = which(apply(data, 1, function(x) all(x == row1)))
index1
data[index1, ]

ggplot(opioid_trends_na,
       aes(x = year, y = average_prevalence, color = age,
           group = interaction(sex, age))) +
  geom_line() +
  facet_wrap(~sex, scales = 'free_y') +
  # Use the panel diagram to show the different age groups
  labs(title = "Trends of Opioid Use Disorders Prevalence in North America by Age,
       x = "Year",
       y = "Average Prevalence (%)",
       color = "Age (years old)") +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)
  )
