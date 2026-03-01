# ============================================================
# Butterworth to Equivalent AR Conversion (MZT)
# ============================================================

#' @noRd
butterworth_poles_MZT <- function(A, N, fs) {

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
    poles_s = s_k,
    poles_z = z_k
  )
}
