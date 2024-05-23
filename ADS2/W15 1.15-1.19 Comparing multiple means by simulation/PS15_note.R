setwd("E:\\A大学\\大二下\\ADS2\\W15 Comparing multiple means by simulation")
jellybeans = read.csv("jellybeans.csv")
jellybeans$colour = as.factor(bean$colour)

compare_pairs <- function(dataset, ndraws) {
  same_group <- {}
  between_group <- {}
  for (i in 1:ndraws) {
    # determine condition (colour) to sample from
    list_colours <- unique(jellybeans$colour)
    colours <- sample(list_colours, 2, replace=FALSE)
    # Make spearate lists for each group
    # (not strictly necessary, but helpful)
    firstgroup <- jellybeans[jellybeans$colour == colours[1], "score"]
    secondgroup <- jellybeans[jellybeans$colour == colours[1], "score"]
    # draw samples
    s1s2 <- sample(firstgroup, 2, replace=FALSE)
    s3 <- sample(secondgroup, 1)
    # compute same group and between group differences, add to list
    same_group <- c(same_group, abs(s1s2[1] - s1s2[2]))
    between_group <- c(between_group, abs(s1s2[2] - s3))
  }
  # compute means of same-group and between-group differences
  mean_same <- mean(same_group)
  mean_between <- mean(between_group)
  # compute absolute difference between those means
  diffmeans = abs(mean_same-mean_between)
  return(diffmeans)
}
our_experiment <- compare_pairs(jellybeans, 10000)
our_experiment
null_distribution <- {}
for (j in 1:1000) {
  # make new dataframe called random_experiment
  # and shuffle colour column
  random_experiment <- jellybeans
  random_experiment$colour <- sample(jellybeans$colour,
                                     nrow(jellybeans), replace=FALSE)
  # use compare_pairs function on random_experiment
  random_meandiff <- compare_pairs(random_experiment, 10000)
  # add the result to the Null distribution
  null_distribution <- c(null_distribution, random_meandiff)
}

hist(null_distribution)

p_value <- sum(null_distribution >= our_experiment) / length(null_distribution)
p_value