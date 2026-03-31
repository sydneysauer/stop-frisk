library(tidyverse)
library(here)
library("penalized")
library("modelr")

source(here("R/ml_functions.R"))

train_data <- read_rds(here("data", "sqf_ml_train.rds"))
holdout_data <- read_rds(here("data", "sqf_ml_holdout.rds"))
 
# ==============================================================================
# EXPLORE AND RESCALE DATA
# ==============================================================================

# IV distributions
glimpse(train_data)
train_data %>%
  select(-arrest) %>%
  summary()

# NAs
train_data %>%
  summarise_all(~ sum(is.na(.)))

# After inspecting data, decided to transform as follows:
    # age, height, weight: standardize (mean 0, sd 1)
    # hour: set to NA if > 24 (some hours are 29, which is likely an error)
train_data <- train_data %>%
  mutate(hour = ifelse(hour > 24, NA, hour),
          age = scale(age),
          height = scale(height),
          weight = scale(weight),
          arrest = ifelse(arrest == TRUE, 1, 0)) %>%
  na.omit() # OMITTING NA FOR NOW SO I CAN RUN THIS STUFF! FIX THIS LATER

# DV distribution
summary(train_data$arrest)

# Gauge accuracy of baseline model on training data
bm <- lm(arrest ~ 1, data = train_data)
calculate_accuracy(bm, validation_set)


# ==============================================================================
# FIT MODELS AND COMPARE ACCURACY
# ==============================================================================

# Create training and validation sets
train_set      <- sample_frac(train_data, 0.7)
validation_set <- anti_join(train_data, train_set, by= "id")
cat("Training set:  ", nrow(train_set), "rows\n")
cat("Validation set: ", nrow(validation_set), "rows\n")

# APPROACH 1: Theoretically informed model
# I think physical stature, time of day, race, gender, and violence will be most predictive.
m1 <- glm(arrest ~ age + height + weight + hour + race + male + reason_violent, data = train_set, family = binomial)
calculate_accuracy(m1, validation_set)
compare_accuracy(bm, m1, train_data) # Only a tiny improvement over baseline! Shows why we need ML...

# Now I'll add all the predictors at once to experiment with overfitting.
X  <- str_c(names(train_data)[!names(train_data) %in% c("arrest", "id")], collapse = " + ")
f2 <- as.formula(str_c("arrest ~ ", X))
m2 <- glm(f2, data = train_set, family = binomial)
calculate_accuracy(m2, validation_set)
compare_accuracy(bm, m2, train_data) # Still getting better than baseline! 
# And weirdly, out sample is better than in sample? Not what I expected!

# What if we go crazy and add an interaction with race?
f3 <- as.formula(str_c("arrest ~ (", X, ") * factor(race)"))
m3 <- glm(f3, data = train_set, family = binomial)
calculate_accuracy(m3, validation_set)
compare_accuracy(m2, m3, train_data)
# Hmm, still not seeing the overfitting trends. In sample and out sample are equal here--and still better than m2. 

# Let's try 5-fold cross validation (going back to m2) to see if that helps.
cross_validate(m2, train_data, k = 5)
# Here, we do see that in-sample (0.208) is better than out-of-sample (0.209).
# But this is still better than the baseline log-loss, showing that model 2 is an improvement! 
# Compared to a single train/validation split, cross validation is more optimistic about in-sample fit and
# less optimistic about out-of-sample fit, which is what we expect. (Woohoo!)
# TODO: Add accuracy back in as a metric. (maybe function calculate_raw_accuracy)
