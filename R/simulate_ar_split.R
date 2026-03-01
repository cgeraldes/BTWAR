#' Simulate an AR(p) Series and Split into Train/Test Sets
#'
#' Generates a stationary AR(p) time series via \code{\link[stats]{arima.sim}},
#' discards a burn-in period, and partitions the result into a training set and
#' a hold-out test set.
#'
#' @param phi_real Numeric vector of true AR coefficients
#'   \eqn{\phi_1, \ldots, \phi_p}.  The process must be stationary (all roots
#'   of the characteristic polynomial outside the unit circle).
#' @param n Integer.  Number of observations to retain after burn-in.
#'   Default \code{2000}.
#' @param burn Integer.  Number of initial observations discarded as burn-in.
#'   Default \code{200}.
#' @param prop_train Numeric in \eqn{(0, 1)}.  Proportion of observations
#'   allocated to the training set.  Default \code{0.8}.
#' @param seasonal Logical.  If \code{TRUE} and \code{freq} is supplied, the
#'   retained series is coerced to a \code{\link[stats]{ts}} object with the
#'   given frequency.  Default \code{FALSE}.
#' @param freq Integer or \code{NA}.  Seasonal frequency passed to
#'   \code{\link[stats]{ts}} when \code{seasonal = TRUE}.  Default \code{NA}.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{\code{series}}{The full simulated series (length \code{n}).}
#'     \item{\code{train}}{Training observations (first
#'       \code{floor(n * prop_train)} values).}
#'     \item{\code{test}}{Hold-out test observations (remaining values).}
#'   }
#'
#' @details
#' The seed is fixed to \code{123} via \code{\link[base]{set.seed}} for
#' reproducibility.  Users who need different realisations should call
#' \code{set.seed()} with their own seed before invoking this function and
#' remove or expose the internal \code{set.seed()} call if the function is
#' integrated into a simulation study.
#'
#' @seealso \code{\link[stats]{arima.sim}}, \code{\link[stats]{ts}}
#'
#' @examples
#' # AR(2) with phi = (0.5, -0.3)
#' result <- simulate_ar_split(phi_real = c(0.5, -0.3))
#' length(result$train)  # 1600
#' length(result$test)   # 400
#'
#' # AR(1) as a monthly time series, 90/10 split
#' result2 <- simulate_ar_split(
#'   phi_real   = 0.8,
#'   n          = 1200,
#'   prop_train = 0.9,
#'   seasonal   = TRUE,
#'   freq       = 12
#' )
#'
#' @importFrom stats arima.sim ts
#' @export
simulate_ar_split <- function(phi_real,
                              n          = 2000L,
                              burn       = 200L,
                              prop_train = 0.8,
                              seasonal   = FALSE,
                              freq       = NA) {

  ## ---- input validation ------------------------------------------------
  if (!is.numeric(phi_real) || length(phi_real) < 1L)
    stop("'phi_real' must be a non-empty numeric vector.")
  if (!is.numeric(n) || length(n) != 1L || n < 2L)
    stop("'n' must be a single integer >= 2.")
  if (!is.numeric(burn) || length(burn) != 1L || burn < 0L)
    stop("'burn' must be a single non-negative integer.")
  if (!is.numeric(prop_train) || length(prop_train) != 1L ||
      prop_train <= 0 || prop_train >= 1)
    stop("'prop_train' must be a single numeric value in (0, 1).")
  if (!is.logical(seasonal) || length(seasonal) != 1L)
    stop("'seasonal' must be a single logical value.")
  if (seasonal && (is.na(freq) || !is.numeric(freq) || freq < 2L))
    stop("'freq' must be an integer >= 2 when 'seasonal = TRUE'.")

  ## ---- simulation ------------------------------------------------------
  set.seed(123L)
  x <- stats::arima.sim(n = n + burn, model = list(ar = phi_real))
  x <- x[seq.int(burn + 1L, n + burn)]

  if (seasonal && !is.na(freq))
    x <- stats::ts(x, frequency = freq)

  ## ---- train / test split ----------------------------------------------
  n_train <- floor(n * prop_train)
  train   <- x[seq_len(n_train)]
  test    <- x[seq.int(n_train + 1L, n)]

  list(series = x, train = train, test = test)
}
