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
#
# Note: 'rmse' and 'mse' are common names. If namespace conflicts
# arise with other packages (e.g., Metrics, yardstick), use
# explicit qualification: BTWAR::rmse(), BTWAR::mse().
# ============================================================


#' Root Mean Squared Error
#'
#' Computes the root mean squared error (RMSE) between observed and
#' predicted values. RMSE penalises large deviations more strongly
#' than MAE due to squaring, and is expressed in the same units as
#' the response variable.
#'
#' \deqn{
#'   \mathrm{RMSE} = \sqrt{\frac{1}{n} \sum_{i=1}^{n} (y_i - \hat{y}_i)^2}
#' }
#'
#' @param y_true Numeric vector of observed (true) values.
#' @param y_pred Numeric vector of predicted values. Must have the same
#'   length as \code{y_true}.
#'
#' @return A single non-negative numeric value.
#'
#' @seealso \code{\link{mse}}
#'
#' @examples
#' y    <- c(1, 2, 3)
#' yhat <- c(1.1, 1.9, 3.2)
#' rmse(y, yhat)
#'
#' @export
rmse <- function(y_true, y_pred) {
  if (!is.numeric(y_true) || !is.numeric(y_pred))
    stop("'y_true' and 'y_pred' must be numeric vectors.")
  if (length(y_true) != length(y_pred))
    stop("'y_true' and 'y_pred' must have the same length.")

  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
}


#' Mean Squared Error
#'
#' Computes the mean squared error (MSE) between observed and predicted
#' values. MSE is commonly used for optimisation and theoretical analysis,
#' and is the square of \code{\link{rmse}}.
#'
#' \deqn{
#'   \mathrm{MSE} = \frac{1}{n} \sum_{i=1}^{n} (y_i - \hat{y}_i)^2
#' }
#'
#' @param y_true Numeric vector of observed (true) values.
#' @param y_pred Numeric vector of predicted values. Must have the same
#'   length as \code{y_true}.
#'
#' @return A single non-negative numeric value.
#'
#' @seealso \code{\link{rmse}}
#'
#' @examples
#' y    <- c(1, 2, 3)
#' yhat <- c(1.1, 1.9, 3.2)
#' mse(y, yhat)
#'
#' @export
mse <- function(y_true, y_pred) {
  if (!is.numeric(y_true) || !is.numeric(y_pred))
    stop("'y_true' and 'y_pred' must be numeric vectors.")
  if (length(y_true) != length(y_pred))
    stop("'y_true' and 'y_pred' must have the same length.")

  mean((y_true - y_pred)^2, na.rm = TRUE)
}
