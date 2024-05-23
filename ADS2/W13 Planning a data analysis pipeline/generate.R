library(tibble)
library(purrr)
setwd("E:\\A大学\\大二上\\ADS2\\week13 12.11-12.17")
# Function to generate synthetic data for a single observation
generate_patient_data <- function() {
  names <- c("John", "Jane", "Bob", "Alice", "Chris")  # Add more names as needed
  genders <- c("Male", "Female")
  provinces <- c("Province A", "Province B", "Province C")
  
  name <- sample(names, 1)
  time_point <- sample(c("Immediate", "3 Weeks"), 1, prob = c(0.5, 0.5), replace = TRUE)
  phone_number <- paste0("555-", sample(100:999, 1), "-", sample(1000:9999, 1))
  gender <- sample(genders, 1)
  home_province <- sample(provinces, 1)
  
  if (time_point == "Immediate") {
    pain_level <- round(rnorm(1, mean = 6, sd = 1), 1)
  } else {
    pain_level <- round(rnorm(1, mean = 2.5, sd = 1), 1)
  }
  
  return(tibble(Name = name, Time_Point = time_point, Phone_Number = phone_number,
                Gender = gender, Home_Province = home_province, Pain_Level = pain_level))
}

# Create a synthetic dataset with multiple observations
num_obs <- 100  # Adjust as needed
synthetic_data <- purrr::map_df(1:num_obs, ~generate_patient_data())

write.csv(synthetic_data, file="data2.csv");