library(tidyverse)
library(here)
library(penalized)
library(modelr)

source(here("R/ml_functions.R"))
set.seed(1235)

train_data <- read_rds(here("data", "sqf_ml_train.rds"))
holdout_data <- read_rds(here("data", "sqf_ml_holdout.rds"))
 
# ==============================================================================
# EXPLORE AND RESCALE DATA (PART A)
# ==============================================================================

# IV distributions
glimpse(train_data)
train_data %>%
  select(-arrest) %>%
  summary()

# NAs
train_data %>%
  summarise_all(~ sum(is.na(.)))

# After inspecting data, decided to further transform as follows:
    # age, height, weight: standardize (mean 0, sd 1)
    # hour: set to NA if > 24 (some hours are 29, which is likely an error)
train_data <- train_data %>%
  mutate(hour = ifelse(hour > 24, NA, hour),
          age = scale(age),
          height = scale(height),
          weight = scale(weight),
          arrest = ifelse(arrest == TRUE, 1, 0)) %>%
  na.omit() # OMITTING NA FOR NOW SO I CAN RUN THIS STUFF! FIX THIS LATER. ASK JOSCHA!!!

# DV distribution
summary(train_data$arrest)

# Gauge accuracy of baseline model on training data
bm <- lm(arrest ~ 1, data = train_data)
calculate_performance(bm, validation_set)
calculate_raw_accuracy(bm, validation_set) 

# ==============================================================================
# FIT MODELS AND COMPARE ACCURACY (PART A)
# ==============================================================================

# Create training and validation sets
train_set      <- sample_frac(train_data, 0.7)
validation_set <- anti_join(train_data, train_set, by= "id")
cat("Training set:  ", nrow(train_set), "rows\n")
cat("Validation set: ", nrow(validation_set), "rows\n")

# Theoretically informed model
# I think physical stature, time of day, race, gender, and violence will be most predictive.
m1 <- glm(arrest ~ age + height + weight + hour + race + male + reason_violent, data = train_set, family = binomial)
calculate_performance(m1, validation_set)
calculate_raw_accuracy(m1, validation_set, cutoff=mean(train_set$arrest)) 
# This coarse accuracy measure isn't helping much (using 0.5 or the mean as cutoff), so I'm going to stick with log-loss going forward.
compare_performance(bm, m1, train_data) # Only a tiny improvement over baseline! Shows why we need ML...

# Now I'll add all the predictors at once to experiment with overfitting.
X  <- str_c(names(train_data)[!names(train_data) %in% c("arrest", "id")], collapse = " + ")
f2 <- as.formula(str_c("arrest ~ ", X))
m2 <- glm(f2, data = train_set, family = binomial)
calculate_performance(m2, validation_set)
compare_performance(bm, m2, train_data) # Still getting better than baseline! 
# And weirdly, out sample is better than in sample? Not what I expected!

# What if we go crazy and add an interaction with race?
f3 <- as.formula(str_c("arrest ~ (", X, ") * factor(race)"))
m3 <- glm(f3, data = train_set, family = binomial)
calculate_performance(m3, validation_set)
compare_performance(m2, m3, train_data)
# Hmm, still not seeing the overfitting trends. In sample and out sample are equal here--and still better than m2. 

# Let's try 5-fold cross validation (going back to m2) to see if that helps.
cross_validate(m2, train_data, k = 5)
# Here, we do see that in-sample (0.208) is better than out-of-sample (0.209).
# But this is still better than the baseline log-loss, showing that model 2 is an improvement! 
# Compared to a single train/validation split, cross validation is more optimistic about in-sample fit and
# less optimistic about out-of-sample fit, which is what we expect. (Woohoo!)

# ==============================================================================
# IMPROVE PREDICTIONS (PART B)
# ==============================================================================

# Goal here: Create three different modeling approaches and compare their performance.

# ATTEMPT 1: FEATURE ENGINEERING EMPHASIS 
# I think there are some transformations I can do to the data that will help the model learn better.

# Create a new variable for whether the stop was in a high crime area (90th percentile+ stops)
precinct_counts <- train_data %>%
  group_by(precinct) %>%
  summarise(stop_count = n()) %>%
  arrange(desc(stop_count)) %>%
  mutate(high_crime = stop_count >= quantile(stop_count, 0.9))
train_data <- train_data %>%
  left_join(precinct_counts %>% select(precinct, high_crime), by = "precinct")

# Transform month into a season factor variable (since crime often seasonal)
train_data <- train_data %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3, 4, 5) ~ "spring",
    month %in% c(6, 7, 8) ~ "summer",
    month %in% c(9, 10, 11) ~ "fall"
  )) %>%
  select(-month) # Drop month so I can still use the same formula structure as before

# Transform time of day into a factor variable for morning, afternoon, evening, night (since not monotonically increasing)
train_data <- train_data %>%
  mutate(time_of_day = case_when(
    hour >= 6 & hour < 12 ~ "morning",
    hour >= 12 & hour < 17 ~ "afternoon",
    hour >= 17 & hour < 21 ~ "evening",
    (hour >= 21 & hour <= 24) | (hour >= 0 & hour < 6) ~ "night"
  )) %>%
  select(-hour)

# Create variable for total number of reasons for stop (since more reasons may indicate higher suspicion)
reason_cols <- grep("^reason_", names(train_data), value = TRUE)
train_data <- train_data %>%
  mutate(num_reasons = rowSums(select(., all_of(reason_cols))))
table(train_data$num_reasons) # Very few stops with more than 4 reasons, so I'll cap it at 4.
train_data <- train_data %>%
  mutate(num_reasons = ifelse(num_reasons > 4, 4, num_reasons))

# Update the train_set, validation_set, and formula to include the new features.
train_set      <- sample_frac(train_data, 0.7)
validation_set <- anti_join(train_data, train_set, by= "id")
cat("Training set:  ", nrow(train_set), "rows\n")
cat("Validation set: ", nrow(validation_set), "rows\n")
X  <- str_c(names(train_data)[!names(train_data) %in% c("arrest", "id")], collapse = " + ")
f4 <- as.formula(str_c("arrest ~ ", X))
m4 <- glm(f4, data = train_set, family = binomial)

# Performance:
appr1 <- cross_validate(m4, validation_set) 
appr1 # Whoa! My new variables actually helped!
compare_performance(bm, m4, train_data) # And much better than baseline!

# ATTEMPT 2: REGULARIZATION EMPHASIS 
# Now, I'll take this new model with all the features and add regularization.

# Start with basic lamba=10 and just a single train/validation split to get a baseline idea
m4_lasso <- penalized(f4, lambda1 = 10, model = "logistic", data = train_set)
appr2 <- cross_validate_penfit(train_data, f4, lambda = 10, k = 5) 
appr2 # Even better than the non-regularized version! Let's continue in this direction.

# Systematically search for optimal regularization strength
models <- tibble(lambda = seq(1, 30, 3)) %>%
  mutate(
    performance = map(lambda, ~ cross_validate_penfit(train_data, f4, lambda = ., k = 5))
  )
# Look at the results
results <- models %>% select(lambda, performance) %>% unnest_wider(performance)
# Plot results to see a pattern
ggplot(results, aes(x = lambda)) +
  geom_line(aes(y = out_sample)) +
  geom_point(aes(y = out_sample)) +
  labs(x = "Lambda", y = "Out of Sample Log Loss", title = "Model Performance by Regularization Strength") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Ok, the graph here is telling me that lambda's optimal value was 1, which suggests to me that I don't have enough
# coefficients! So, in my next approach, I'm going to add a whole bunch more features and interactions.

# TODO: Officially deal with missing data before I add any new features. Re run all code to be certain!


# ==============================================================================
# GENERATE COMPETITION SUBMISSION (PART B)
# ==============================================================================

# Questions for Joscha:
# 1. How to make sure all the transformations I did to the training data are also done to holdout? function with crazy validation?
    # can recode the holdout data with the same exact code, since I have all the Xs
# 2. What to do about missing values? I omitted them for now so I could run the code...
    # remember: our goal is out of sample predictive performance!
    # so if it seems useful, I can create new features based on missingness, or create a category of "unknown"
    # it's actually OK to predict with separate models! (eg, I can predict NO ARREST for any missing hehe)
      # justify this by saying that when I removed the missings, the % arrested went up substantially, meaning
      # people with missing values are less likely to be arrested
    # maybe even make a crosstab of missingness and arrest to show this?

# TODO go back and edit my annotations based on now having the random seed set.