# Prediction-evaluation metrics (log loss, accuracy, AUC, RMSE, R^2, MSE)

validate_metric_inputs <- function(predicted, actual, fn_name) {
  if (length(predicted) != length(actual)) {
    cli::cli_abort(c(
      "Error in HockeyModel::{fn_name}()",
      "x" = "Expected {.arg predicted} and {.arg actual} to have the same length, got {length(predicted)} and {length(actual)} instead.",
      "i" = "Please retry with vectors of equal length."
    ))
  }
}


#' Log Loss Calculator
#'
#' @param predicted Predicted odds of an event occuring
#' @param actual If the event occured (0 or 1), or model results
#' in 0, 0.25, 0.4, 0.6, 0.75, 1.0
#'
#' @return a log loss value for the event(s)
#' @export
logLoss <- function(predicted, actual) {
  validate_metric_inputs(predicted, actual, "logLoss")
  predicted[predicted == 0] <- 1e-15
  predicted[predicted == 1] <- 1 - 1e-15

  actual <- as.numeric(actual > 0.5)

  ll <- (actual * log(predicted)) + ((1 - actual) * log(1 - predicted))

  return(-(sum(ll) / length(predicted)))
}


#' Accuracy Calculator
#'
#' @param predicted Predicted odds of an event occuring. needen't be of set (0,1)
#' @param actual If the event occured (0 or 1), or model results in 0, 0.25, 0.4, 0.6, 0.75, 1.0
#'
#' @return a percentage of correct predictions
#' @export
accuracy <- function(predicted, actual) {
  validate_metric_inputs(predicted, actual, "accuracy")

  predicted <- as.numeric(predicted > 0.5)
  actual <- as.numeric(actual > 0.5)

  accuracy <- sum(as.numeric(predicted == actual)) / length(predicted)

  return(accuracy)
}


#' AUC
#' @description calculate the AUC metrics. From MLMetrics
#'
#' @param predicted Predicted odds of an event occurring. needen't be of set (0,1)
#' @param actual If the event occurred (0 or 1), or model results in 0, 0.25, 0.4, 0.6, 0.75, 1.0
#'
#' @return a single value for auc
#' @export
auc <- function(predicted, actual) {
  validate_metric_inputs(predicted, actual, "auc")

  actual <- as.numeric(actual > 0.5)

  rank <- rank(predicted)
  n_positive <- sum(actual > 0.5)
  n_negative <- sum(actual < 0.5)

  auc <- (sum(rank[actual > 0.5]) - n_positive * (n_positive + 1) / 2) /
    (n_positive * n_negative)

  return(auc)
}


#' RMSE
#' @description calculate the RMSE metrics. From MLMetrics
#'
#' @param predicted Predicted numeric value
#' @param actual Actual numeric value
#'
#' @return a single value for RMSE
#' @export
rmse <- function(predicted, actual) {
  validate_metric_inputs(predicted, actual, "rmse")

  return(sqrt(mean((actual - predicted)^2)))
}


#' R Square
#' @description calculate the R^2 metrics. From MLMetrics
#'
#' @param predicted Predicted numeric value
#' @param actual Actual numeric value
#'
#' @return a single value for R^2
#' @export
rsquare <- function(predicted, actual) {
  validate_metric_inputs(predicted, actual, "rsquare")

  return(stats::cor(predicted, actual)^2)
}

#' MSE
#' @description calculate the MSE metrics. From MLMetrics
#'
#' @param predicted Predicted numeric value
#' @param actual Actual numeric value
#'
#' @return a single value for MSE
#' @export
mse <- function(predicted, actual) {
  validate_metric_inputs(predicted, actual, "mse")

  return(mean((actual - predicted)^2))
}
