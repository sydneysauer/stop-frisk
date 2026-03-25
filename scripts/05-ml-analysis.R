library(tidyverse)
library(here)

train_data <- read_rds(here("data", "sqf_ml_train.rds"))
holdout_data <- read_rds(here("data", "sqf_ml_holdout.rds"))
 
###### EXPLORE AND RESCALE DATA ######

# IV distributions
glimpse(train_data)
train_data %>%
  select(-arrest) %>%
  summary()

# After inspecting data, decided to transform as follows:
    # age, height, weight: standardize (mean 0, sd 1)
    # hour: set to NA if > 24 (some hours are 29, which is likely an error)
train_data <- train_data %>%
  mutate(hour = ifelse(hour > 24, NA, hour),
          age = scale(age),
          height = scale(height),
          weight = scale(weight))

# DV distribution
summary(train_data$arrest) # Highly skewed, only 3.5% of stops result in arrest