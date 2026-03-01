# ============================================================
# Spectral Analysis Utilities for BTWAR
# ============================================================


#' Compute the One-Sided Power Spectrum
#'
#' Estimates the one-sided power spectrum of a time series using the
#' Fast Fourier Transform (FFT). The series is mean-centred prior to
#' transformation to remove the DC component.
#'
#' Let \eqn{X_k} denote the \eqn{k}-th coefficient of the DFT of
#' \eqn{x_1, \ldots, x_n}. The raw periodogram is defined as
#'
#' \deqn{P_k = \frac{|X_k|^2}{n}, \quad k = 0, 1, \ldots, \lfloor n/2 \rfloor - 1}
#'
#' and the corresponding frequencies are
#'
#' \deqn{f_k = \frac{k \cdot f_s}{n}}
#'
#' Only the non-redundant (one-sided) portion of the spectrum is returned,
#' i.e. frequencies from \eqn{0} up to (but not including) the Nyquist
#' frequency \eqn{f_s / 2}.
#'
#' @param x Numeric vector. The input time series. Must contain at least
#'   2 observations.
#' @param fs Positive numeric. Sampling frequency in Hz (or consistent
#'   units). Default \code{1}.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{f}}{Numeric vector of frequencies
#'       (\eqn{0} to \eqn{f_s/2}, exclusive), length
#'       \eqn{\lfloor n/2 \rfloor}.}
#'     \item{\code{P}}{Numeric vector of power spectral estimates,
#'       same length as \code{f}.}
#'   }
#'
#' @seealso \code{\link[stats]{fft}}, \code{\link[stats]{spectrum}}
#'
#' @importFrom stats fft
#'
#' @examples
#' set.seed(1)
#' x   <- arima.sim(n = 256, model = list(ar = 0.8))
#' sp  <- compute_spectrum(x, fs = 1)
#' plot(sp$f, sp$P, type = "l",
#'      xlab = "Frequency (Hz)", ylab = "Power",
#'      main = "One-sided power spectrum")
#'
#' @export
compute_spectrum <- function(x, fs = 1) {

  # ---- input validation --------------------------------------------------
  if (!is.numeric(x) || length(x) < 2L)
    stop("'x' must be a numeric vector with at least 2 observations.")
  if (!is.numeric(fs) || length(fs) != 1L || fs <= 0)
    stop("'fs' must be a single positive number.")

  # ---- mean-centring -----------------------------------------------------
  x <- x - mean(x, na.rm = TRUE)

  # ---- FFT and periodogram -----------------------------------------------
  n   <- length(x)
  X   <- stats::fft(x)
  P   <- Mod(X)^2 / n
  f   <- seq.int(0L, n - 1L) * fs / n

  # ---- one-sided (non-redundant) portion ---------------------------------
  idx <- seq_len(floor(n / 2L))

  list(f = f[idx], P = P[idx])
}
