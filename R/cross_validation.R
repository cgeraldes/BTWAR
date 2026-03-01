# ============================================================
# Rolling-Origin Cross-Validation for BTWAR
# ============================================================

#' @noRd
cv_butterworth_AsN <- function(
    y,
    As_vec,
    N_vec,
    fs,
    min_train = NULL,
    verbose = TRUE,
    method
){

  nobs <- length(y)

  grid <- expand.grid(N = N_vec, A = As_vec)
  ngrid <- nrow(grid)

  compute_one <- function(i){

    N <- grid$N[i]
    A <- grid$A[i]

    if(verbose)
      message(sprintf("Testing N = %d | A = %g dB", N, A))

    out <- butterworth_poles_MZT(A, N, fs)

    z_k <- out$poles_z
    a   <- Re(pracma::Poly(z_k))
    phi <- -a[-1]

    p <- length(phi)

    t0 <- if(is.null(min_train)) max(20 * p, 100) else min_train

    if(t0 >= nobs - 5){

      return(list(
        rmse = NA_real_,
        result = list(
          N = N,
          A = A,
          rmse_cv = NA_real_,
          phi = phi,
          poles_z = z_k,
          fc = out$fc
        )
      ))
    }

    erros <- numeric(nobs - t0)
    k <- 1

    for(t in t0:(nobs - 1)){

      y_train <- y[1:t]
      y_test  <- y[t + 1]

      yhat <- yhat_ar(y_train, phi)

      if(is.na(yhat[t])) next

      idx <- !is.na(yhat)

      alpha <- alpha_estimate(
        x = y_train[idx],
        yhat = yhat[idx],
        method = method
      )

      y_pred <- alpha * yhat[t]

      erros[k] <- y_test - y_pred
      k <- k + 1
    }

    erros <- erros[1:(k - 1)]
    rmse_val <- sqrt(mean(erros^2))

    list(
      rmse = rmse_val,
      result = list(
        N = N,
        A = A,
        rmse_cv = rmse_val,
        phi = phi,
        poles_z = z_k,
        fc = out$fc
      )
    )
  }

  out <- lapply(seq_len(ngrid), compute_one)

  rmse_cv <- vapply(out, `[[`, numeric(1), "rmse")
  results <- lapply(out, `[[`, "result")

  best_i <- which.min(ifelse(is.na(rmse_cv), Inf, rmse_cv))

  list(
    grid    = grid,
    rmse_cv = rmse_cv,
    best    = results[[best_i]],
    best_i  = best_i,
    best_N  = grid$N[best_i],
    best_A  = grid$A[best_i],
    all     = results
  )
}
