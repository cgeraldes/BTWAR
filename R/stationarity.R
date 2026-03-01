# ============================================================
# Stationarity Transformation Utilities for BTWAR
# ============================================================
#
# BTWAR requires stationary input series. This file provides
# tools for testing and enforcing stationarity via successive
# differencing guided by the Augmented Dickey-Fuller test.
# ============================================================


#' @noRd
make_stationary <- function(x, max_d = 3L, alpha = 0.05) {

  # ---- input validation --------------------------------------------------
  x <- as.numeric(x)
  if (length(x) < 4L)
    stop("'x' must have at least 4 observations for the ADF test.")
  if (!is.numeric(max_d) || length(max_d) != 1L || max_d < 0L)
    stop("'max_d' must be a single non-negative integer.")
  if (!is.numeric(alpha) || length(alpha) != 1L ||
      alpha <= 0 || alpha >= 1)
    stop("'alpha' must be a single numeric value in (0, 1).")

  # ---- iterative differencing --------------------------------------------
  d   <- 0L
  adf <- tseries::adf.test(x)

  while (adf$p.value > alpha && d < max_d) {
    x   <- diff(x)
    d   <- d + 1L
    adf <- tseries::adf.test(x)
  }

  list(
    x_stationary = x,
    d            = d,
    p_value      = adf$p.value,
    stationary   = (adf$p.value <= alpha)
  )
}


#' Apply Stationarity Transformation to a Train/Test Split
#'
#' Determines the differencing order required to stationarise the training
#' series using \code{make_stationary()} and applies the same order to the
#' test series, ensuring both sets are on a comparable scale.
#'
#' This function is a convenience wrapper intended for use before fitting a
#' \code{\link{btwar_fit}} model when the user wants to inspect or control
#' the stationarity step explicitly. Internally, \code{\link{btwar_fit}}
#' calls \code{make_stationary()} directly.
#'
#' @param y_tr_raw Numeric vector. Raw training time series. Must contain
#'   at least 4 observations.
#' @param y_te_raw Numeric vector. Raw test (hold-out) time series. Must
#'   contain at least 1 observation.
#' @param max_d Non-negative integer. Maximum differencing order passed to
#'   \code{make_stationary()}. Default \code{3}.
#' @param alpha Numeric in \eqn{(0, 1)}. Significance level for the
#'   Augmented Dickey-Fuller test. Default \code{0.05}.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{\code{y_tr}}{Numeric vector. Stationarised training series
#'       of length \eqn{n_{tr} - d}.}
#'     \item{\code{y_te}}{Numeric vector. Differenced test series of
#'       length \eqn{n_{te} - d}.}
#'     \item{\code{d}}{Integer. Number of differences applied.}
#'     \item{\code{stationary}}{Logical. \code{TRUE} if the ADF test
#'       declared the training series stationary.}
#'   }
#'
#' @seealso \code{\link{btwar_fit}}, \code{\link[tseries]{adf.test}}
#'
#' @examples
#' set.seed(1)
#' y     <- cumsum(rnorm(200))          # random walk (non-stationary)
#' n_tr  <- 160
#' split <- apply_stationarity(
#'   y_tr_raw = y[seq_len(n_tr)],
#'   y_te_raw = y[seq.int(n_tr + 1L, length(y))]
#' )
#' split$d           # differencing order applied
#' split$stationary  # was stationarity achieved?
#'
#' @importFrom tseries adf.test
#'
#' @export
apply_stationarity <- function(y_tr_raw,
                               y_te_raw,
                               max_d = 3L,
                               alpha = 0.05) {

  # ---- input validation --------------------------------------------------
  if (!is.numeric(y_tr_raw) || length(y_tr_raw) < 4L)
    stop("'y_tr_raw' must be a numeric vector with at least 4 observations.")
  if (!is.numeric(y_te_raw) || length(y_te_raw) < 1L)
    stop("'y_te_raw' must be a non-empty numeric vector.")
  if (!is.numeric(max_d) || length(max_d) != 1L || max_d < 0L)
    stop("'max_d' must be a single non-negative integer.")
  if (!is.numeric(alpha) || length(alpha) != 1L ||
      alpha <= 0 || alpha >= 1)
    stop("'alpha' must be a single numeric value in (0, 1).")

  # ---- transform training series -----------------------------------------
  stat   <- make_stationary(y_tr_raw, max_d = max_d, alpha = alpha)
  y_tr   <- stat$x_stationary
  d_used <- stat$d

  # ---- apply same differencing to test series ----------------------------
  y_te <- y_te_raw
  if (d_used > 0L) {
    for (i in seq_len(d_used)) y_te <- diff(y_te)
  }

  list(
    y_tr       = y_tr,
    y_te       = y_te,
    d          = d_used,
    stationary = stat$stationary
  )
}
