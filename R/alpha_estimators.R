# ============================================================
# BTWAR Scaling Parameter Estimators
# ============================================================
#
# The BTWAR model assumes:
#
#   x_t ≈ alpha * yhat_t
#
# where yhat_t is produced by the Butterworth-equivalent AR model.
#
# The parameter alpha rescales the AR prediction amplitude and
# minimizes prediction error under different robustness assumptions.
#
# This file contains several estimators for alpha.
# ============================================================


#' Least Squares Scaling Estimator
#'
#' Computes the scaling parameter using ordinary least squares (OLS).
#'
#' This estimator minimizes the squared error:
#'
#'   sum((x - alpha * yhat)^2)
#'
#' It is optimal under Gaussian noise assumptions.
#'
#' @param x Numeric vector of observed values.
#' @param yhat Numeric vector of predicted values.
#'
#' @return Numeric scalar representing the estimated scaling parameter.
#'
#' @examples
#' x <- rnorm(100)
#' yhat <- rnorm(100)
#' alpha_ls(x, yhat)
#'
#' @export
alpha_ls <- function(x, yhat) {

  sum(x * yhat, na.rm = TRUE) /
    sum(yhat^2, na.rm = TRUE)

}


#' Least Absolute Deviations Scaling Estimator
#'
#' Computes a robust scaling parameter using the weighted median
#' of the ratios x / yhat.
#'
#' This estimator minimizes the absolute error:
#'
#'   sum(|x - alpha * yhat|)
#'
#' It is more robust to outliers than least squares.
#'
#' @param x Numeric vector of observed values.
#' @param yhat Numeric vector of predicted values.
#'
#' @return Numeric scalar representing the estimated scaling parameter.
#'
#' @examples
#' x <- rnorm(100)
#' yhat <- rnorm(100)
#' alpha_lad(x, yhat)
#'
#' @export
alpha_lad <- function(x, yhat) {

  idx <- abs(yhat) > 0

  ratios <- x[idx] / yhat[idx]

  weights <- abs(yhat[idx])

  o <- order(ratios)

  ratios <- ratios[o]
  weights <- weights[o]

  cw <- cumsum(weights) / sum(weights)

  ratios[which(cw >= 0.5)[1]]

}


#' Huber Scaling Estimator
#'
#' Computes a robust scaling parameter using the Huber loss function.
#'
#' The Huber estimator provides a compromise between least squares
#' and least absolute deviations. It behaves like least squares for
#' small residuals and like LAD for large residuals.
#'
#' Estimation is performed using iterative reweighted least squares (IRLS).
#'
#' @param x Numeric vector of observed values.
#' @param yhat Numeric vector of predicted values.
#' @param delta Huber threshold parameter. Default is 1.345.
#' @param tol Convergence tolerance. Default is 1e-6.
#' @param max_iter Maximum number of iterations. Default is 100.
#'
#' @return Numeric scalar representing the estimated scaling parameter.
#'
#' @examples
#' x <- rnorm(100)
#' yhat <- rnorm(100)
#' alpha_huber(x, yhat)
#'
#' @export
alpha_huber <- function(
    x,
    yhat,
    delta = 1.345,
    tol = 1e-6,
    max_iter = 100
) {

  alpha <- alpha_ls(x, yhat)

  for (k in seq_len(max_iter)) {

    e <- x - alpha * yhat

    w <- ifelse(abs(e) <= delta, 1, delta / abs(e))

    alpha_new <-
      sum(w * x * yhat, na.rm = TRUE) /
      sum(w * yhat^2, na.rm = TRUE)

    if (abs(alpha_new - alpha) < tol)
      break

    alpha <- alpha_new
  }

  alpha
}


#' General Scaling Parameter Interface
#'
#' Provides a unified interface for selecting the scaling
#' parameter estimation method used in BTWAR.
#'
#' @param x Numeric vector of observed values.
#' @param yhat Numeric vector of predicted values.
#' @param method Character string specifying estimation method.
#'   One of:
#'   \itemize{
#'     \item{"ls"}{Least squares}
#'     \item{"lad"}{Least absolute deviations}
#'     \item{"huber"}{Huber robust estimator}
#'   }
#' @param w Optional weights (currently unused).
#' @param delta Huber threshold parameter.
#'
#' @return Numeric scalar representing the estimated scaling parameter.
#'
#' @examples
#' x <- rnorm(100)
#' yhat <- rnorm(100)
#' alpha_estimate(x, yhat, method = "ls")
#'
#' @export
alpha_estimate <- function(
    x,
    yhat,
    method = c("ls", "lad", "huber"),
    w = NULL,
    delta = 1.345
) {

  method <- match.arg(method)

  switch(
    method,
    ls    = alpha_ls(x, yhat),
    lad   = alpha_lad(x, yhat),
    huber = alpha_huber(x, yhat, delta = delta)
  )
}
