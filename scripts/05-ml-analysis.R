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


# ==============================================================================
# FIT MODELS AND COMPARE ACCURACY (PART A)
# ==============================================================================

# Create training and validation sets
train_set      <- sample_frac(train_data, 0.7)
validation_set <- anti_join(train_data, train_set, by= "id")
cat("Training set:  ", nrow(train_set), "rows\n")
cat("Validation set: ", nrow(validation_set), "rows\n")

# Gauge accuracy of baseline model on training data
bm <- lm(arrest ~ 1, data = train_data)
calculate_performance(bm, validation_set)
calculate_raw_accuracy(bm, validation_set) 

# Theoretically informed model
# I think physical stature, time of day, race, gender, and violence will be most predictive.
m1 <- glm(arrest ~ age + height + weight + hour + race + male + reason_violent, data = train_set, family = binomial)
calculate_performance(m1, validation_set)
calculate_raw_accuracy(m1, validation_set, cutoff=mean(train_set$arrest)) 
# This coarse accuracy measure isn't helping much (using 0.5 or the mean as cutoff), so I'm going to stick with log-loss going forward.
compare_performance(bm, m1, validation_set) # Only a tiny improvement over baseline! Shows why we need ML...

# Now I'll add all the predictors at once to experiment with overfitting.
X  <- str_c(names(train_data)[!names(train_data) %in% c("arrest", "id")], collapse = " + ")
f2 <- as.formula(str_c("arrest ~ ", X))
m2 <- glm(f2, data = train_set, family = binomial)
calculate_performance(m2, validation_set)
compare_performance(bm, m2, validation_set) # Still getting better than baseline! 
# And weirdly, out sample is better than in sample? Not what I expected!

# What if we go crazy and add an interaction with race?
f3 <- as.formula(str_c("arrest ~ (", X, ") * factor(race)"))
m3 <- glm(f3, data = train_set, family = binomial)
calculate_performance(m3, validation_set)
# Hmm, still not seeing the overfitting trends. Out sample still better. Overall performance worse than m2.

# Let's try 5-fold cross validation to see if that helps better compare m2 and m3.
cross_validate(m2, train_data, k = 5)
cross_validate(m3, train_data, k = 5)
# Here, we do see that in-sample (0.208) is better than out-of-sample (0.209).
# Compared to a single train/validation split, cross validation is more optimistic about in-sample fit and
# less optimistic about out-of-sample fit, which is what we expect. (Woohoo!)

# Model 3 has better in-sample but worse out-of-sample, indicating that the race interaction is overfitting.

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
appr1 # Matches the m2 out of sample, but not an improvememt.

# ATTEMPT 2: REGULARIZATION EMPHASIS 
# Now, I'll take this new model with all the features and add regularization.

# Start with basic lamba=10 
m4_lasso <- penalized(f4, lambda1 = 10, model = "logistic", data = train_set)
appr2 <- cross_validate_penfit(train_data, f4, lambda = 10, k = 5) 
appr2 # Mildly better than the non-regularized version! Let's continue in this direction.

# Systematically search for optimal regularization strength
models <- tibble(lambda = seq(1, 40, 4)) %>%
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

# Ok, the graph here is telling me that lambda's optimal value is 5, so let's try that
m4_lasso_5 <- penalized(f4, lambda1 = 5, model = "logistic", data = train_set)
appr2a <- cross_validate_penfit(train_data, f4, lambda = 5, k = 5) 
appr2a # not really different...

# ATTEMPT 3: INTERACTIONS EMPHASIS 
# I will add lots of interaction terms to give my regularization something more to chew on.

# I think race could be predictive interaction term, as above. 
# Note: I previously added "inside" as another interaction term, but this wasn't helpful. 
# I was also getting a glm warning here that "glm.fit: fitted probabilities numerically 0 or 1 occurred"
# which Google says may indicate overfitting. 

# I'm keeping the features I added in the approaches above, since they were helpful.
f5 <- as.formula(str_c("arrest ~ (", X, ") * factor(race)"))
m5 <- glm(f5, data = train_set, family = binomial)

appr3 <- cross_validate(m5, validation_set)
appr3 # In sample is better than previous models, out of sample is WAY worse (yep, overfitting!)

# ATTEMPT 4: INTERACTIONS WITH REGULARIZATION
# Cut down on overfitting from the crazy model above with regularization.

# Again, start with basic lamba=10
# This kept getting stuck, so I implemented a max iterations to get close enough
m5_lasso <- penalized(f5, lambda1 = 10, model = "logistic", data = train_set, maxiter=500)
appr4 <- cross_validate_penfit(validation_set, f5, lambda = 10, k = 5) 
appr4 # This still isn't beating the simple model 2, which just has all the predictors.

# Let's see if different lambda values might help...
models <- tibble(lambda = seq(5, 25, 2)) %>%
  mutate(
    performance = map(lambda, ~ cross_validate_penfit(train_data, f5, lambda = ., k = 5))
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

# Here, the minimum log loss seems to be around lambda = 15, so let's try that 
m6_lasso <- penalized(f5, lambda1 = 15, model = "logistic", data = train_set, maxiter=500)
appr5 <- cross_validate_penfit(validation_set, f5, lambda = 15, k = 5) 
appr5 # Ack, out of sample is worse!!!! 0.21??!?!

# I think it's time to go back to model 4 lasso.

# ==============================================================================
# GENERATE COMPETITION SUBMISSION (PART B)
# ==============================================================================

# After much trial and error, model 4 lasso with lamba=10 is my best bet! 
# Let's apply it to the holdout set.

# 0. Save a copy of all the holdout IDs, because I'll be dropping a lot of the missing rows
holdout_ids <- holdout_data %>% select(id)

# 1. Apply transformations and feature engineering to holdout data
holdout_data <- holdout_data %>%
  mutate(hour = ifelse(hour > 24, NA, hour),
          age = scale(age),
          height = scale(height),
          weight = scale(weight)) %>%
  na.omit() %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3, 4, 5) ~ "spring",
    month %in% c(6, 7, 8) ~ "summer",
    month %in% c(9, 10, 11) ~ "fall"
  )) %>%
  select(-month) %>%
  mutate(time_of_day = case_when(
    hour >= 6 & hour < 12 ~ "morning",
    hour >= 12 & hour < 17 ~ "afternoon",
    hour >= 17 & hour < 21 ~ "evening",
    (hour >= 21 & hour <= 24) | (hour >= 0 & hour < 6) ~ "night"
  )) %>%
  select(-hour) %>%
  mutate(num_reasons = rowSums(select(., all_of(reason_cols)))) %>%
  mutate(num_reasons = ifelse(num_reasons > 4, 4, num_reasons)) %>%
  left_join(precinct_counts %>% select(precinct, high_crime), by = "precinct")

# peek at it make sure it looks good
glimpse(holdout_data)

# 2. Generate predictions with the best model (m4_lasso with lambda = 10)
holdout_preds <- predict(m4_lasso, newdata = holdout_data, type = "response")

# 3. Merge predictions with holdout IDs and fill in missing values (CHECK THIS CODE)
# AT the end: merge all predictions from the actual prediction, then fill in missing with the mean 
#     (of training data? or of predicted probs?).


# 4. Validate and submit
source("scripts/validate-submission.R")