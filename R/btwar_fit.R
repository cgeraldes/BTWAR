# ============================================================
# BTWAR Model Fitting and S3 Methods
# ============================================================

#' Fit a Butterworth-Induced Autoregressive Model
#'
#' Fits a Butterworth-Induced Autoregressive (BTWAR) model using
#' nested rolling-origin cross-validation for order and attenuation
#' parameter selection.
#'
#' @param y_tr_raw Numeric vector. Training time series.
#' @param y_te_raw Numeric vector. Test (hold-out) time series.
#' @param fs Positive numeric. Sampling frequency. Default \code{2}.
#' @param method Character string. Scaling estimation method, one of
#'   \code{"ls"} (least squares), \code{"lad"} (least absolute deviations),
#'   or \code{"huber"} (Huber M-estimator). Default \code{"ls"}.
#' @param As_vec Numeric vector. Grid of stopband attenuation values (dB)
#'   to search over. Default \code{seq(20, 60, by = 5)}.
#' @param N_vec Integer vector. Grid of AR orders to search over.
#'   Default \code{2:20}.
#' @param max_d Non-negative integer. Maximum differencing order allowed
#'   during the stationarity transformation step. Default \code{3}.
#' @param alpha_stationarity Numeric in \eqn{(0, 1)}. Significance level
#'   for the Augmented Dickey-Fuller stationarity test. Default \code{0.05}.
#' @param seed Integer. Random seed for reproducibility. Default \code{123}.
#'
#' @return An object of class \code{"btwar"}, which is a named list
#'   containing:
#'   \describe{
#'     \item{\code{parameters}}{List with \code{d_used}, \code{N_opt},
#'       \code{A_opt}, \code{fc_opt}, \code{alpha}, and \code{phi}.}
#'     \item{\code{performance}}{List with \code{rmse_train} and
#'       \code{rmse_test}.}
#'     \item{\code{train}}{List with \code{y_real} and \code{yhat} for
#'       the training set.}
#'     \item{\code{test}}{List with \code{y_real} and \code{yhat} for
#'       the test set.}
#'     \item{\code{mu}}{Training mean used for centering.}
#'     \item{\code{call}}{The matched call.}
#'   }
#'
#' @seealso \code{\link{predict.btwar}}, \code{\link{summary.btwar}}
#'
#' @examples
#' \dontrun{
#' result <- btwar_fit(
#'   y_tr_raw = train_series,
#'   y_te_raw = test_series,
#'   fs       = 2,
#'   method   = "ls"
#' )
#' summary(result)
#' }
#'
#' @export
btwar_fit <- function(y_tr_raw,
                      y_te_raw,
                      fs                 = 2,
                      method             = "ls",
                      As_vec             = seq(20, 60, by = 5),
                      N_vec              = 2:20,
                      max_d              = 3L,
                      alpha_stationarity = 0.05,
                      seed               = 123L) {

  # ---- input validation --------------------------------------------------
  if (!is.numeric(y_tr_raw) || length(y_tr_raw) < 10L)
    stop("'y_tr_raw' must be a numeric vector with at least 10 observations.")

  if (!is.numeric(y_te_raw) || length(y_te_raw) < 1L)
    stop("'y_te_raw' must be a non-empty numeric vector.")

  if (!is.numeric(fs) || length(fs) != 1L || fs <= 0)
    stop("'fs' must be a single positive number.")

  method <- match.arg(method, c("ls", "lad", "huber"))

  if (!is.numeric(As_vec) || length(As_vec) < 1L)
    stop("'As_vec' must be a non-empty numeric vector.")

  if (!is.numeric(N_vec) || length(N_vec) < 1L || any(N_vec < 1L))
    stop("'N_vec' must be a non-empty vector of positive integers.")

  if (!is.numeric(max_d) || length(max_d) != 1L || max_d < 0L)
    stop("'max_d' must be a single non-negative integer.")

  if (!is.numeric(alpha_stationarity) || length(alpha_stationarity) != 1L ||
      alpha_stationarity <= 0 || alpha_stationarity >= 1)
    stop("'alpha_stationarity' must be a single numeric value in (0, 1).")

  set.seed(seed)

  # ---- stationarity transformation ---------------------------------------
  st    <- make_stationary(x = y_tr_raw, max_d = max_d,
                           alpha = alpha_stationarity)
  y_tr  <- st$x_stationary
  d_used <- st$d

  y_te <- y_te_raw
  if (d_used > 0L) {
    for (i in seq_len(d_used)) y_te <- diff(y_te)
  }

  # ---- centering ---------------------------------------------------------
  mu_tr <- mean(y_tr, na.rm = TRUE)
  y_tr  <- y_tr - mu_tr
  y_te  <- y_te - mu_tr

  # ---- cross-validation --------------------------------------------------
  cv_res <- cv_butterworth_AsN(
    y      = y_tr,
    As_vec = As_vec,
    N_vec  = N_vec,
    fs     = fs,
    method = method
  )

  phi_BTW <- cv_res$best$phi
  N_opt   <- cv_res$best$N
  A_opt   <- cv_res$best$A
  fc_opt  <- cv_res$best$fc
  p       <- length(phi_BTW)

  if (length(y_tr) <= p)
    stop("Series too short for the selected model order.")

  t0 <- p + 1L

  # ---- training predictions ----------------------------------------------
  y0_tr     <- yhat_ar(y_tr, phi_BTW)
  idx_tr    <- seq.int(t0, length(y_tr))
  y_real_tr <- y_tr[idx_tr]
  y0_tr     <- y0_tr[idx_tr]

  alpha_tr <- alpha_estimate(x = y_real_tr, yhat = y0_tr, method = method)
  yhat_tr  <- alpha_tr * y0_tr
  rmse_tr  <- rmse(y_real_tr, yhat_tr)

  # ---- test predictions --------------------------------------------------
  y0_te     <- yhat_ar(y_te, phi_BTW)
  idx_te    <- seq.int(t0, length(y_te))
  y_real_te <- y_te[idx_te]
  y0_te     <- y0_te[idx_te]

  yhat_te  <- alpha_tr * y0_te
  rmse_te  <- rmse(y_real_te, yhat_te)

  # ---- assemble output ---------------------------------------------------
  structure(
    list(
      parameters = list(
        d_used = d_used,
        N_opt  = N_opt,
        A_opt  = A_opt,
        fc_opt = fc_opt,
        alpha  = alpha_tr,
        phi    = phi_BTW
      ),
      performance = list(
        rmse_train = rmse_tr,
        rmse_test  = rmse_te
      ),
      train = list(y_real = y_real_tr, yhat = yhat_tr),
      test  = list(y_real = y_real_te, yhat = yhat_te),
      mu    = mu_tr,
      call  = match.call()
    ),
    class = "btwar"
  )
}


# ============================================================
# S3 Methods
# ============================================================

#' Print a BTWAR Model
#'
#' Compact console display of a fitted \code{"btwar"} object.
#'
#' @param x Object of class \code{"btwar"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print btwar
#' @export
print.btwar <- function(x, ...) {
  cat("Butterworth-Induced Autoregressive Model\n")
  cat("------------------------------------------\n")
  cat("Order (N)          :", x$parameters$N_opt,  "\n")
  cat("Attenuation (As)   :", x$parameters$A_opt,  "\n")
  cat("Cutoff frequency   :", x$parameters$fc_opt, "\n")
  cat("Differencing order :", x$parameters$d_used, "\n")
  cat("\nRMSE (train) :", round(x$performance$rmse_train, 4), "\n")
  cat("RMSE (test)  :", round(x$performance$rmse_test,  4), "\n")
  invisible(x)
}


#' Summarise a BTWAR Model
#'
#' Returns a \code{"summary.btwar"} object with model parameters and
#' performance metrics. The associated \code{print} method displays a
#' formatted summary including the AR coefficients.
#'
#' @param object Object of class \code{"btwar"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"summary.btwar"} (invisibly).
#'
#' @method summary btwar
#' @export
summary.btwar <- function(object, ...) {
  out <- structure(
    list(
      parameters  = object$parameters,
      performance = object$performance
    ),
    class = "summary.btwar"
  )
  print(out)
  invisible(out)
}


#' Print a BTWAR Model Summary
#'
#' @param x Object of class \code{"summary.btwar"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print summary.btwar
#' @export
print.summary.btwar <- function(x, ...) {
  cat("Butterworth-Induced Autoregressive Model\n")
  cat("------------------------------------------\n")
  cat("Order (N)          :", x$parameters$N_opt,  "\n")
  cat("Attenuation (As)   :", x$parameters$A_opt,  "\n")
  cat("Cutoff frequency   :", x$parameters$fc_opt, "\n")
  cat("Differencing order :", x$parameters$d_used, "\n")
  cat("\nRMSE (train) :", round(x$performance$rmse_train, 4), "\n")
  cat("RMSE (test)  :", round(x$performance$rmse_test,  4), "\n")
  cat("\nAR Coefficients:\n")
  print(round(x$parameters$phi, 6))
  invisible(x)
}


#' Predict from a BTWAR Model
#'
#' Generates one-step-ahead predictions for a new time series using a
#' fitted \code{"btwar"} model.
#'
#' @param object Object of class \code{"btwar"}.
#' @param newdata Numeric vector. New time series to predict from.
#' @param ... Additional arguments (currently unused).
#'
#' @return Numeric vector of predicted values.
#'
#' @method predict btwar
#' @export
predict.btwar <- function(object, newdata, ...) {
  if (!is.numeric(newdata) || length(newdata) < 1L)
    stop("'newdata' must be a non-empty numeric vector.")

  phi   <- object$parameters$phi
  alpha <- object$parameters$alpha
  d     <- object$parameters$d_used
  mu    <- object$mu

  x <- newdata
  if (d > 0L) {
    for (i in seq_len(d)) x <- diff(x)
  }
  x <- x - mu

  alpha * yhat_ar(x, phi)
}


#' Extract AR Coefficients from a BTWAR Model
#'
#' @param object Object of class \code{"btwar"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Named numeric vector of AR coefficients.
#'
#' @method coef btwar
#' @export
coef.btwar <- function(object, ...) {
  object$parameters$phi
}


#' Fitted Values from a BTWAR Model
#'
#' @param object Object of class \code{"btwar"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Numeric vector of fitted values on the training set.
#'
#' @method fitted btwar
#' @export
fitted.btwar <- function(object, ...) {
  object$train$yhat
}


#' Residuals from a BTWAR Model
#'
#' @param object Object of class \code{"btwar"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Numeric vector of residuals from the training set.
#'
#' @method residuals btwar
#' @export
residuals.btwar <- function(object, ...) {
  object$train$y_real - object$train$yhat
}
