# ============================================================
# Performance Metrics for BTWAR
# ============================================================
#
# This file contains error metrics used for evaluating
# prediction performance in the BTWAR framework.
#
# These metrics are primarily used during:
#   - Rolling-origin cross-validation
#   - Model comparison
#   - Train/test performance reporting
# ============================================================


#' Root Mean Squared Error (RMSE)
#'
#' Computes the root mean squared error between observed and predicted values.
#'
#' RMSE measures the average magnitude of prediction error and
#' penalizes large deviations more strongly due to squaring.
#'
#' Mathematically:
#'
#' \deqn{
#'   RMSE = \sqrt{\frac{1}{n} \sum (y_i - \hat{y}_i)^2}
#' }
#'
#' @param y_true Numeric vector of observed (true) values.
#' @param y_pred Numeric vector of predicted values.
#'
#' @return Numeric scalar representing the root mean squared error.
#'
#' @examples
#' y <- c(1, 2, 3)
#' yhat <- c(1.1, 1.9, 3.2)
#' rmse(y, yhat)
#'
#' @export
rmse <- function(y_true, y_pred) {

  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))

}


#' Mean Squared Error (MSE)
#'
#' Computes the mean squared error between observed and predicted values.
#'
#' MSE is commonly used for optimization and theoretical analysis.
#'
#' Mathematically:
#'
#' \deqn{
#'   MSE = \frac{1}{n} \sum (y_i - \hat{y}_i)^2
#' }
#'
#' @param y Numeric vector of observed values.
#' @param yhat Numeric vector of predicted values.
#'
#' @return Numeric scalar representing the mean squared error.
#'
#' @examples
#' y <- c(1, 2, 3)
#' yhat <- c(1.1, 1.9, 3.2)
#' mse(y, yhat)
#'
#' @export
mse <- function(y, yhat) {

  mean((y - yhat)^2, na.rm = TRUE)

}
