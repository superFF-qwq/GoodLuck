library(dplyr)
# Screen and diagnosis
str(substance_use)
class(substance_use)
head(substance_use)
summary(substance_use)

# Check for missing values
any(is.na(substance_use))
substance_use <- substance_use %>% 
  filter(!is.na(val) & !is.na(upper) & !is.na(lower))

# Check for duplicates
any(duplicated(substance_use))
substance_use <- substance_use %>% 
  distinct()

# Abnormal data processing
substance_use <- substance_use %>% 
  filter(val >= 0 & val <= 1 & 
           upper >= 0 & upper <= 1 & 
           lower >= 0 & lower <= 1)
