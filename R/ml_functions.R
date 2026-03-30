#' @title log_loss
#' @description Calculate the log loss (cross-entropy loss) for binary classification
#' @param actual A numeric vector of actual binary labels (0 or 1)
#' @param predicted A numeric vector of predicted probabilities (between 0 and 1)
#' @return A numeric value representing the log loss
log_loss <- function(actual, predicted) {
  eps <- 1e-15
  predicted = pmin(pmax(predicted, eps), 1 - eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

#' @title calculate_accuracy
#' @description Calculate the accuracy (log-loss) of a classification model for any given dataset
#' @param model A classification model object
#' @param data A data frame containing the data for prediction
#' @return Log loss value representing the accuracy of the model on the given data
calculate_accuracy <- function(model, newdata) {
  y_name <- names(model.frame(model))[1]
  print(y_name)
  y      <- newdata[[y_name]]
  y_hat  <- predict(model, newdata = newdata, type = "response")
  
  log_loss(y, y_hat)
}

#' @title compare_accuracy
#' @description Compare the accuracy (log-loss) of two classification models on a given dataset
#' @param model1 A classification model object (e.g., baseline model)
#' @param model2 A classification model object (e.g., new model)
#' @param data A data frame containing the data for prediction
#' @return A printed summary of the log-loss for both models and the percentage improvement of model2 over model1
compare_accuracy <- function(model1, model2, data) {
  # TODO return to this and convert to variable # models later if needed
  acc1 <- calculate_accuracy(model1, data)
  acc2 <- calculate_accuracy(model2, data)
  improvement <- (acc1 - acc2) / acc1
  cat("Model 1 Log-Loss: ", round(acc1, 4), "\n")
  cat("Model 2 Log-Loss: ", round(acc2, 4), "\n")
  cat("Improvement over Model 1: ", round(improvement * 100, 2), "%\n")
}

#' @title report_accuracy
#' @description Report the in-sample and out-of-sample log-loss for a classification model
#' @param in_sample_acc A numeric value representing the in-sample log-loss
#' @param out_sample_acc A numeric value representing the out-of-sample log-loss
#' @return A printed summary of the in-sample and out-of-sample log-loss
report_accuracy <- function(in_sample_acc, out_sample_acc) {
  improvement <- (in_sample_acc - out_sample_acc) / in_sample_acc
  cat("In-Sample Log-Loss: ", round(in_sample_acc, 3), "\n")
  cat("Out-of-Sample Log-Loss: ", round(out_sample_acc, 3), "\n")
}
