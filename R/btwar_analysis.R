# ============================================================
# Main BTWAR Analysis Pipeline
# ============================================================

#' Run BTWAR Analysis
#'
#' Executes the full BTWAR modeling pipeline:
#' stationarity transformation, parameter selection,
#' model fitting, and performance evaluation.
#'
#' @param y_tr_raw Training series.
#' @param y_te_raw Test series.
#' @param fs Sampling frequency.
#' @param method Scaling estimation method.
#' @param As_vec Attenuation grid.
#' @param N_vec Order grid.
#' @param max_d Maximum differencing order.
#' @param alpha_stationarity ADF significance level.
#' @param seed Random seed.
#'
#' @return Structured list containing parameters and performance metrics.
#'
#' @export
run_btwar_analysis <- function(
    y_tr_raw,
    y_te_raw,
    fs = 2,
    method = "ls",
    As_vec = seq(20, 60, by = 5),
    N_vec  = 2:20,
    max_d = 3,
    alpha_stationarity = 0.05,
    seed = 123
){

  set.seed(seed)

  st <- make_stationary(
    x = y_tr_raw,
    max_d = max_d,
    alpha = alpha_stationarity
  )

  y_tr <- st$x_stationary
  d_used <- st$d

  y_te <- y_te_raw
  if(d_used > 0){
    for(i in seq_len(d_used)){
      y_te <- diff(y_te)
    }
  }

  mu_tr <- mean(y_tr, na.rm = TRUE)

  y_tr <- y_tr - mu_tr
  y_te <- y_te - mu_tr

  cv_res <- cv_butterworth_AsN(
    y      = y_tr,
    As_vec = As_vec,
    N_vec  = N_vec,
    fs     = fs,
    method = method
  )

  phi_BTW <- cv_res$best$phi
  N_opt   <- cv_res$best$N
  A_opt   <- cv_res$best$A
  fc_opt  <- cv_res$best$fc

  p <- length(phi_BTW)
  t0 <- p + 1

  y0_tr <- yhat_ar(y_tr, phi_BTW)
  y_real_tr <- y_tr[t0:length(y_tr)]
  y0_tr <- y0_tr[t0:length(y_tr)]

  alpha_tr <- alpha_estimate(
    x = y_real_tr,
    yhat = y0_tr,
    method = method
  )

  yhat_tr <- alpha_tr * y0_tr
  rmse_tr <- rmse(y_real_tr, yhat_tr)

  y0_te <- yhat_ar(y_te, phi_BTW)
  y_real_te <- y_te[t0:length(y_te)]
  y0_te <- y0_te[t0:length(y_te)]

  yhat_te <- alpha_tr * y0_te
  rmse_te <- rmse(y_real_te, yhat_te)
  list(
    parameters = list(
      d_used = d_used,
      N_opt  = N_opt,
      A_opt  = A_opt,
      fc_opt = fc_opt,
      alpha  = alpha_tr,
      phi    = phi_BTW
    ),
    performance = list(
      rmse_train = rmse_tr,
      rmse_test  = rmse_te
    ),
    train = list(
      y_real = y_real_tr,
      yhat   = yhat_tr
    ),
    test = list(
      y_real = y_real_te,
      yhat   = yhat_te
    ),
    cv = cv_res
  )
}
