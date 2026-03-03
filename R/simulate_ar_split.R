#' Simulate an AR(p) Series and Split into Train/Test Sets
#'
#' Generates a stationary AR(p) time series via \code{\link[stats]{arima.sim}},
#' discards a burn-in period, and partitions the result into a training set and
#' a hold-out test set.
#'
#' Random number generation is controlled externally by the user via
#' \code{\link[base]{set.seed}} when reproducibility is required.
#'
#' @param phi_real Numeric vector of true AR coefficients
#'   \eqn{\phi_1, \ldots, \phi_p}. The process must be stationary (all roots
#'   of the characteristic polynomial outside the unit circle).
#' @param n Integer. Number of observations to retain after burn-in.
#'   Default \code{2000}.
#' @param burn Integer. Number of initial observations discarded as burn-in.
#'   Default \code{200}.
#' @param prop_train Numeric in \eqn{(0, 1)}. Proportion of observations
#'   allocated to the training set. Default \code{0.8}.
#' @param seasonal Logical. If \code{TRUE} and \code{freq} is supplied, the
#'   retained series is coerced to a \code{\link[stats]{ts}} object with the
#'   given frequency. Default \code{FALSE}.
#' @param freq Integer or \code{NA}. Seasonal frequency passed to
#'   \code{\link[stats]{ts}} when \code{seasonal = TRUE}. Default \code{NA}.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{\code{series}}{The full simulated series (length \code{n}).}
#'     \item{\code{train}}{Training observations (first
#'       \code{floor(n * prop_train)} values).}
#'     \item{\code{test}}{Hold-out test observations (remaining values).}
#'     \item{\code{poles}}{Complex vector of true AR poles in the Z-plane.}
#'   }
#'
#' @details
#' The function does not modify the global random number generator state.
#' For reproducible results, users should call \code{set.seed()} prior
#' to invoking this function.
#'
#' @seealso \code{\link[stats]{arima.sim}}, \code{\link[stats]{ts}}
#'
#' @examples
#' # Reproducible AR(3) with phi = (0.6, -0.3, 0.2)
#' set.seed(123)
#' result <- simulate_ar_split(phi_real = c(0.6, -0.3, 0.2))
#' length(result$train)  # 1600
#' length(result$test)   # 400
#'
#' # AR(3) as a monthly time series, 90/10 split
#' set.seed(123)
#' result2 <- simulate_ar_split(
#'   phi_real   = c(0.6, -0.3, 0.2),
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
  p <- length(phi_real)
  message("Simulating AR(", p, ") series ...", appendLF = FALSE)

  x <- stats::arima.sim(n = n + burn, model = list(ar = phi_real))
  x <- x[seq.int(burn + 1L, n + burn)]

  if (seasonal && !is.na(freq))
    x <- stats::ts(x, frequency = freq)

  message(" done.")

  ## ---- train / test split ----------------------------------------------
  n_train <- floor(n * prop_train)
  train   <- x[seq_len(n_train)]
  test    <- x[seq.int(n_train + 1L, n)]

  ## ---- true AR poles in the Z-plane ------------------------------------
  poles <- poles_AR(phi_real)

  ## ---- output ----------------------------------------------------------
  list(
    series = x,
    train  = train,
    test   = test,
    poles  = poles
  )
}
