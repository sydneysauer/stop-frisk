#' @title prediction_performance_lm
#' @description Calculate in-sample and out-of-sample R-squared for a linear model
#' @param model A linear model object
#' @param newdata A data frame containing the new data for prediction
#' @return A named vector with in-sample and out-of-sample R-squared values
prediction_performance_lm <- function(model, newdata) {
  y_name <- names(model.frame(model))[1]
  y      <- newdata[[y_name]]
  y_hat  <- predict(model, newdata = newdata)
  SSR    <- sum((y - y_hat)^2)
  SST    <- sum((y - mean(y))^2)
  r_sq   <- 1 - SSR / SST
  c("in-sample"    = summary(model)$r.squared,
    "out-of-sample" = r_sq)
}

#' @title calculate_accuracy
#' @description Calculate the accuracy of a classification model based on a specified threshold
#' @param model A classification model object
#' @param data A data frame containing the data for prediction
#' @param threshold A numeric value between 0 and 1 to classify predictions (default
#' is 0.5)
#' @return A numeric value representing the accuracy of the model
calculate_accuracy <- function(model, data, threshold = 0.5) {
  y_name <- names(model.frame(model))[1]
  y      <- data[[y_name]]
  y_hat  <- predict(model, newdata = data, type = "response")
  y_pred  <- ifelse(y_hat >= threshold, 1, 0)
  sprintf("Accuracy: %.2f%%", mean(y_pred == y) * 100)
  mean(y_pred == y)
}