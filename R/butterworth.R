# ============================================================
# Butterworth to Equivalent AR Conversion (MZT)
# ============================================================

#' Butterworth Filter to Equivalent AR Poles (MZT)
#'
#' Converts an analog Butterworth low-pass filter into
#' discrete-time poles using the Matched Z-Transform (MZT).
#'
#' These poles define the equivalent autoregressive (AR)
#' representation used in the BTWAR framework.
#'
#' @param A Stopband attenuation in dB.
#' @param N Filter order (positive integer).
#' @param fs Sampling frequency.
#'
#' @return A list containing filter parameters and poles.
#'
#' @examples
#' butterworth_polos_MZT(A = 40, N = 4, fs = 2)
#'
#' @export
butterworth_polos_MZT <- function(A, N, fs) {

  T <- 1 / fs
  f_nyquist <- fs / 2

  Hs <- 10^(-A / 10)

  fc <- f_nyquist / ((1 / Hs - 1)^(1 / (2 * N)))
  wc <- 2 * pi * fc

  k <- seq_len(N)

  s_k <- wc * exp(1i * (pi/2 + (2 * k - 1) * pi / (2 * N)))
  z_k <- exp(s_k * T)

  list(
    A_dB    = A,
    N       = N,
    fs      = fs,
    fc      = fc,
    wc      = wc,
    polos_s = s_k,
    polos_z = z_k
  )
}
