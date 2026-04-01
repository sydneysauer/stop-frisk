#' @title log_loss
#' @description Calculate the log loss (cross-entropy loss) for binary classification
#' @param actual A numeric vector of actual binary labels (0 or 1)
#' @param predicted A numeric vector of predicted probabilities (between 0 and 1)
#' @return A numeric value representing the log loss
log_loss <- function(actual, predicted) {
  eps <- 1e-15
  predicted = pmin(pmax(predicted, eps), 1 - eps) # Clip predicted probabilities to avoid log(0)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

#' @title calculate_performance
#' @description Calculate the in-sample and out-of-sample performance (log-loss) of a 
#' classification model
#' @param model A classification model object
#' @param newdata A data frame containing the data for out-of-sample prediction
#' @return In sample and out of sample log-loss values
calculate_performance <- function(model, newdata) {
  # Calculate out-of-sample log loss
  y_name <- names(model.frame(model))[1]
  print(y_name)
  y      <- newdata[[y_name]]
  y_hat  <- predict(model, newdata = newdata, type = "response")
  out_samp <- log_loss(y, y_hat)

  # Calculate in-sample log loss
  in_samp <- log_loss(model$model[[y_name]], model$fitted.values)
  c("in_sample" = in_samp, "out_sample" = out_samp)
}

#' @title calculate_raw_accuracy
#' @description Calculate the raw accuracy (proportion of correct classifications) for a classification model
#' @param model A classification model object
#' @param newdata A data frame containing the data for prediction
#' @param cutoff A numeric value representing the cutoff for classification (default is 0.5)
#' @return A numeric value representing the raw accuracy
calculate_raw_accuracy <- function(model, newdata, cutoff=0.5) {
  # Calculate out-of-sample acccuracy
  y_name <- names(model.frame(model))[1]
  print(y_name)
  y      <- newdata[[y_name]]
  y_hat  <- predict(model, newdata = newdata, type = "response")
  out_samp <- ifelse(y_hat > cutoff, 1, 0)

  # Calculate in-sample accuracy
  in_samp <- ifelse(model$fitted.values > cutoff, 1, 0)

  c("in_sample" = mean(model$model[[y_name]] == in_samp), "out_sample" = mean(y == out_samp))
}


#' @title compare_performance
#' @description Compare the performance (log-loss) of two classification models
#' @param model1 A classification model object (e.g., baseline model)
#' @param model2 A classification model object (e.g., new model)
#' @param data A data frame containing the data for prediction
#' @return A printed summary of the log-loss for both models and the percentage improvement of model2 over model1
compare_performance <- function(model1, model2, data) {
  # TODO return to this and convert to variable # models later if needed
  acc1 <- calculate_performance(model1, data)
  acc2 <- calculate_performance(model2, data)
  bind_rows(acc1, acc2) %>%
    mutate(model = c("model1", "model2")) %>%
    select(model, everything()) %>%
    print()
}

#' @title cross_validate
#' @description Perform k-fold cross-validation for a classification model
#' @param model A classification model object
#' @param data A data frame containing the data for cross-validation
#' @param k The number of folds for cross-validation (default is 5)
#' @return A data frame containing the log-loss for each fold (in and out of sample)
cross_validate <- function(model, data, k = 5) {
  folds <- crossv_kfold(data, k)
  fold_results <- folds %>%
    mutate(
      model = map(train, ~ update(model, data = .x)),
      acc   = map2(model, test, ~ calculate_performance(.x, as.data.frame(.y)))
    ) %>%
    tidyr::unnest_wider(acc)
    c("in_sample" = mean(fold_results$in_sample), "out_sample" = mean(fold_results$out_sample))
}

calculate_performance_penfit <- function(model, data, newdata) {
  y_name <- as.character(model@formula$penalized)[[2]]
  # In-sample log loss
  y_train <- data[[y_name]]
  # TODO REVISE THIS

  # Out-of-sample R²
  y_test  <- newdata[[y_name]]
  y_hat   <- predict(model, data = newdata)[, 1]
  SSR_out <- sum((y_test - y_hat)^2)
  SST_out <- sum((y_test - mean(y_test))^2)
  r_sq_out <- 1 - SSR_out / SST_out
  c("in-sample" = r_sq_in, "out-of-sample" = r_sq_out)
}