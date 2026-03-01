# ============================================================
# Spectral Analysis Utilities
# ============================================================


#' Compute Power Spectrum
#'
#' Computes the one-sided power spectrum using the Fast Fourier Transform.
#'
#' @param x Time series.
#' @param fs Sampling frequency.
#'
#' @return List with frequency vector and power spectrum.
#' @export
compute_spectrum <- function(x, fs) {

  x <- x - mean(x, na.rm = TRUE)

  n <- length(x)

  X <- stats::fft(x)

  P <- Mod(X)^2 / n

  f <- (0:(n - 1)) * fs / n

  idx <- seq_len(floor(n / 2))

  list(
    f = f[idx],
    P = P[idx]
  )
}
