# ============================================================
# Autoregressive Prediction Utilities
# ============================================================


#' Compute Z-Plane Poles of an AR Model
#'
#' Returns the poles of an autoregressive (AR) model in the complex Z-plane
#' by finding the roots of the characteristic polynomial
#' \eqn{1 - \phi_1 z^{-1} - \cdots - \phi_p z^{-p}}.
#'
#' @param phi Numeric vector of AR coefficients
#'   \eqn{(\phi_1, \phi_2, \ldots, \phi_p)}, ordered from lag 1 to lag
#'   \eqn{p}.
#'
#' @return A complex vector of length \eqn{p} containing the Z-plane poles.
#'   A model is stable if and only if all poles lie strictly inside the unit
#'   circle, i.e., \code{all(Mod(poles_AR(phi)) < 1)}.
#'
#' @details
#' The characteristic polynomial is
#' \deqn{A(z) = 1 - \phi_1 z^{-1} - \cdots - \phi_p z^{-p},}
#' which, after multiplication by \eqn{z^p}, becomes the polynomial whose
#' roots are computed by \code{\link[base]{polyroot}}. Coefficients are
#' reversed internally so that \code{polyroot} receives them in ascending
#' degree order.
#'
#' @seealso \code{\link{yhat_ar}}, \code{\link{yhat_arma}},
#'   \code{\link{plot_zpoles}}, \code{\link[base]{polyroot}}
#'
#' @export
#'
#' @examples
#' # AR(2) with phi = c(0.6, -0.3)
#' phi <- c(0.6, -0.3)
#' poles <- poles_AR(phi)
#' print(poles)
#' all(Mod(poles) < 1)  # TRUE => stable
#'
#' # Use with plot_zpoles
#' \dontrun{
#' df <- data.frame(Re = Re(poles), Im = Im(poles))
#' plot_zpoles(fit, external_list = list("AR(2) poles" = df))
#' }
poles_AR <- function(phi) {
  if (!is.numeric(phi) || length(phi) < 1L)
    stop("'phi' must be a non-empty numeric vector.")
  polyroot(rev(c(1, -phi)))
}


#' One-Step-Ahead AR Predictions
#'
#' Computes one-step-ahead predictions for a time series under a pure
#' autoregressive model of order \eqn{p}.
#'
#' @param x Numeric vector. The observed time series.
#' @param phi Numeric vector of AR coefficients
#'   \eqn{(\phi_1, \phi_2, \ldots, \phi_p)}, ordered from lag 1 to lag
#'   \eqn{p}.
#'
#' @return A numeric vector of the same length as \code{x}. The first
#'   \eqn{p} elements are \code{NA} (insufficient history); element
#'   \eqn{t} (\eqn{t > p}) contains
#'   \eqn{\hat{x}_t = \phi_1 x_{t-1} + \cdots + \phi_p x_{t-p}}.
#'
#' @details
#' No intercept is included. Center \code{x} before calling this function
#' if the series has a non-zero mean.
#'
#' @seealso \code{\link{poles_AR}}, \code{\link{yhat_arma}},
#'   \code{\link{btwar_fit}}
#'
#' @export
#'
#' @examples
#' x   <- as.numeric(arima.sim(list(ar = c(0.6, -0.3)), n = 200))
#' phi <- c(0.6, -0.3)
#' yh  <- yhat_ar(x, phi)
#'
#' # First p values are NA
#' head(yh, 5)
#'
#' # RMSE on the predictable portion
#' obs <- x[(length(phi) + 1):length(x)]
#' hat <- yh[(length(phi) + 1):length(x)]
#' sqrt(mean((obs - hat)^2))
yhat_ar <- function(x, phi) {
  if (!is.numeric(x) || length(x) < 1L)
    stop("'x' must be a non-empty numeric vector.")
  if (!is.numeric(phi) || length(phi) < 1L)
    stop("'phi' must be a non-empty numeric vector.")

  p    <- length(phi)
  n    <- length(x)
  yhat <- rep(NA_real_, n)

  if (n <= p)
    return(yhat)

  for (t in (p + 1L):n) {
    yhat[t] <- sum(phi * rev(x[(t - p):(t - 1L)]))
  }
  yhat
}


#' One-Step-Ahead ARMA Predictions
#'
#' Computes one-step-ahead predictions for a time series under an
#' autoregressive moving-average (ARMA) model of orders \eqn{(p, q)}.
#'
#' @param x Numeric vector. The observed time series.
#' @param ar Numeric vector of AR coefficients
#'   \eqn{(\phi_1, \ldots, \phi_p)}, ordered from lag 1 to lag \eqn{p}.
#'   Supply \code{numeric(0)} for a pure MA model.
#' @param ma Numeric vector of MA coefficients
#'   \eqn{(\theta_1, \ldots, \theta_q)}, ordered from lag 1 to lag \eqn{q}.
#'   Supply \code{numeric(0)} for a pure AR model.
#'
#' @return A numeric vector of the same length as \code{x}. Elements
#'   \eqn{1, \ldots, \max(p, q)} are \code{NA}; element \eqn{t} contains
#'   \deqn{\hat{x}_t = \sum_{i=1}^{p} \phi_i x_{t-i}
#'                    + \sum_{j=1}^{q} \theta_j \varepsilon_{t-j},}
#'   where residuals \eqn{\varepsilon_s = x_s - \hat{x}_s} are accumulated
#'   recursively and initialised to zero.
#'
#' @details
#' Residuals prior to the start index are initialised to zero. No intercept
#' is included; center \code{x} before calling if the series has a non-zero
#' mean. For a pure AR model, prefer \code{\link{yhat_ar}}, which is
#' slightly more efficient.
#'
#' @seealso \code{\link{yhat_ar}}, \code{\link{poles_AR}},
#'   \code{\link{btwar_fit}}
#'
#' @export
#'
#' @examples
#' x  <- as.numeric(arima.sim(list(ar = 0.6, ma = 0.4), n = 200))
#' yh <- yhat_arma(x, ar = 0.6, ma = 0.4)
#'
#' # First max(p, q) values are NA
#' head(yh, 5)
#'
#' # RMSE on the predictable portion
#' start <- max(length(0.6), length(0.4)) + 1L
#' sqrt(mean((x[start:length(x)] - yh[start:length(x)])^2))
yhat_arma <- function(x, ar, ma) {
  if (!is.numeric(x) || length(x) < 1L)
    stop("'x' must be a non-empty numeric vector.")
  if (!is.numeric(ar))
    stop("'ar' must be a numeric vector (use numeric(0) for no AR terms).")
  if (!is.numeric(ma))
    stop("'ma' must be a numeric vector (use numeric(0) for no MA terms).")

  p     <- length(ar)
  q     <- length(ma)
  n     <- length(x)
  yhat  <- rep(NA_real_, n)
  e     <- rep(0,        n)
  start <- max(p, q) + 1L

  if (n <= start)
    return(yhat)

  for (t in start:n) {
    ar_part  <- if (p > 0L) sum(ar * rev(x[(t - p):(t - 1L)])) else 0
    ma_part  <- if (q > 0L) sum(ma * rev(e[(t - q):(t - 1L)])) else 0
    yhat[t]  <- ar_part + ma_part
    e[t]     <- x[t] - yhat[t]
  }
  yhat
}
