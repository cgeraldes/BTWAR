# ============================================================
# Autoregressive Prediction Utilities
# ============================================================


#' Compute AR Model Poles
#'
#' Computes the roots of the AR characteristic polynomial.
#'
#' @param phi Vector of AR coefficients.
#'
#' @return Complex vector of roots.
#' @export
polos_AR <- function(phi) {
  polyroot(rev(c(1, -phi)))
}


#' Generate AR One-Step-Ahead Predictions
#'
#' Computes recursive one-step-ahead predictions
#' from an autoregressive model.
#'
#' @param x Time series.
#' @param phi AR coefficient vector.
#'
#' @return Numeric vector of predictions.
#' @export
yhat_ar <- function(x, phi) {

  p <- length(phi)
  n <- length(x)

  yhat <- rep(NA_real_, n)

  if (n <= p)
    return(yhat)

  for (t in (p + 1):n) {
    yhat[t] <- sum(phi * rev(x[(t - p):(t - 1)]))
  }

  yhat
}


#' Generate ARMA Predictions
#'
#' Computes recursive ARMA predictions.
#'
#' @param x Time series.
#' @param ar AR coefficients.
#' @param ma MA coefficients.
#'
#' @return Numeric vector of predictions.
#' @export
yhat_arma <- function(x, ar, ma){

  p <- length(ar)
  q <- length(ma)
  n <- length(x)

  yhat <- rep(NA_real_, n)
  e    <- rep(0, n)

  start <- max(p, q) + 1

  if (n <= start)
    return(yhat)

  for (t in start:n) {

    ar_part <- sum(ar * rev(x[(t - p):(t - 1)]))
    ma_part <- sum(ma * rev(e[(t - q):(t - 1)]))

    yhat[t] <- ar_part + ma_part
    e[t]    <- x[t] - yhat[t]
  }

  yhat
}
