# ============================================================
# AR Dataset Construction
# ============================================================

#' Construct AR Supervised Dataset
#'
#' Converts a univariate time series into a supervised learning
#' dataset suitable for autoregressive modeling.
#'
#' @param series Numeric time series.
#' @param p AR order.
#' @param train_frac Training fraction (0-1).
#' @param scale Logical; whether to standardize series.
#'
#' @return List containing training/test splits and scaling info.
#'
#' @export
make_AR_dataset <- function(
    series,
    p = 5,
    train_frac = 0.7,
    scale = TRUE
){

  stopifnot(is.numeric(series))
  stopifnot(p >= 1)

  n <- length(series)

  if(scale){

    mu <- mean(series, na.rm = TRUE)
    sdv <- stats::sd(series, na.rm = TRUE)
    s <- (series - mu) / sdv

  } else {

    mu <- 0
    sdv <- 1
    s <- series
  }

  X <- stats::embed(s, p + 1)[, -1]
  y <- stats::embed(s, p + 1)[, 1]

  N <- nrow(X)
  train_N <- floor(train_frac * N)

  X_train <- X[seq_len(train_N), , drop = FALSE]
  y_train <- y[seq_len(train_N)]

  X_test  <- X[(train_N + 1):N, , drop = FALSE]
  y_test  <- y[(train_N + 1):N]

  list(
    treino  = X_train,
    y_train = y_train,
    X_test  = X_test,
    y_test  = y_test,
    mean    = mu,
    sd      = sdv
  )
}
