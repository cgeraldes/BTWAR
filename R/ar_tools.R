# ============================================================
# Autoregressive Prediction Utilities
# ============================================================

#' @noRd
poles_AR <- function(phi) {
  polyroot(rev(c(1, -phi)))
}


#' @noRd
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


#' @noRd
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
