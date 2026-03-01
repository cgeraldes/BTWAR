#' @noRd
alpha_ls <- function(x, yhat) {
  sum(x * yhat, na.rm = TRUE) /
    sum(yhat^2, na.rm = TRUE)
}

#' @noRd
alpha_lad <- function(x, yhat) {
  idx     <- abs(yhat) > 0
  ratios  <- x[idx] / yhat[idx]
  weights <- abs(yhat[idx])
  o       <- order(ratios)
  ratios  <- ratios[o]
  weights <- weights[o]
  cw      <- cumsum(weights) / sum(weights)
  ratios[which(cw >= 0.5)[1]]
}

#' @noRd
alpha_huber <- function(x,
                        yhat,
                        delta    = 1.345,
                        tol      = 1e-6,
                        max_iter = 100L) {
  alpha <- alpha_ls(x, yhat)
  for (k in seq_len(max_iter)) {
    e         <- x - alpha * yhat
    w         <- ifelse(abs(e) <= delta, 1, delta / abs(e))
    alpha_new <- sum(w * x * yhat, na.rm = TRUE) /
      sum(w * yhat^2,   na.rm = TRUE)
    if (abs(alpha_new - alpha) < tol) break
    alpha <- alpha_new
  }
  alpha
}

#' @noRd
alpha_estimate <- function(x,
                           yhat,
                           method = c("ls", "lad", "huber"),
                           delta  = 1.345) {
  method <- match.arg(method)
  switch(method,
         ls    = alpha_ls(x, yhat),
         lad   = alpha_lad(x, yhat),
         huber = alpha_huber(x, yhat, delta = delta)
  )
}
