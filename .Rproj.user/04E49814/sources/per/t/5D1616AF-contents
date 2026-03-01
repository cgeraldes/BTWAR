# ============================================================
# Stationarity Transformation Utilities
# ============================================================
#
# BTWAR requires stationary time series.
# This file provides tools for testing and enforcing stationarity
# using differencing and the Augmented Dickey-Fuller test.
# ============================================================


#' Make Time Series Stationary
#'
#' Applies successive differencing until stationarity is achieved
#' according to the Augmented Dickey-Fuller (ADF) test.
#'
#' Differencing stops when either:
#'   - The ADF test rejects the unit root hypothesis, or
#'   - The maximum number of differences is reached.
#'
#' @param x Numeric time series.
#' @param max_d Maximum number of differences allowed. Default is 3.
#' @param alpha Significance level for ADF test. Default is 0.05.
#'
#' @return A list containing:
#'   \item{x_stationary}{Stationary transformed series}
#'   \item{d}{Number of differences applied}
#'   \item{p_value}{Final ADF test p-value}
#'   \item{stationary}{Logical indicator}
#'
#' @examples
#' set.seed(1)
#' x <- cumsum(rnorm(200))
#' make_stationary(x)
#'
#' @export
make_stationary <- function(x, max_d = 3, alpha = 0.05){

  x <- as.numeric(x)
  d <- 0

  adf <- tseries::adf.test(x)

  while(adf$p.value > alpha && d < max_d){
    x <- diff(x)
    d <- d + 1
    adf <- tseries::adf.test(x)
  }

  list(
    x_stationary = x,
    d = d,
    p_value = adf$p.value,
    stationary = (adf$p.value <= alpha)
  )
}


#' Apply Stationarity to Train/Test Split
#'
#' Applies stationarity transformation to training data
#' and applies the same differencing order to test data.
#'
#' @param y_tr_raw Training time series.
#' @param y_te_raw Test time series.
#' @param max_d Maximum differencing order.
#' @param alpha Significance level.
#'
#' @return List containing stationary train/test series and order used.
#'
#' @export
apply_stationarity <- function(y_tr_raw, y_te_raw, max_d = 3, alpha = 0.05) {

  stat <- make_stationary(y_tr_raw, max_d, alpha)

  y_tr <- stat$x_stationary
  d_used <- stat$d

  y_te <- y_te_raw

  if (d_used > 0) {
    for (i in seq_len(d_used)) {
      y_te <- diff(y_te)
    }
  }

  list(
    y_tr = y_tr,
    y_te = y_te,
    d = d_used,
    stationary = stat$stationary
  )
}
