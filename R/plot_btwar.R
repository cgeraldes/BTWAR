# ============================================================
# Plot Method for BTWAR Objects
# ============================================================

# Suppress R CMD check NOTE for ggplot2 non-standard evaluation variables
utils::globalVariables(c("time", "Series", "Value"))


#' Plot Predictions from a BTWAR Model
#'
#' Generates a \pkg{ggplot2} line chart of fitted or test-set predictions
#' from a \code{"btwar"} object. Optionally overlays the observed series
#' and/or an external comparison series (e.g., a benchmark model).
#'
#' @param x Object of class \code{"btwar"}, as returned by
#'   \code{\link{btwar_fit}}.
#' @param dataset Character string. Which dataset to display: \code{"train"}
#'   (default) or \code{"test"}.
#' @param fs Positive numeric. Sampling frequency used to construct the time
#'   axis as \eqn{t = 0, 1/f_s, 2/f_s, \ldots}. Default \code{1}.
#' @param show_observed Logical. If \code{TRUE} (default), the observed
#'   series is overlaid on the predicted values.
#' @param external Optional numeric vector of the same length as the selected
#'   dataset. If supplied, it is plotted as an additional comparison series.
#'   Default \code{NULL}.
#' @param external_name Character string. Legend label for the external
#'   series. Default \code{"External"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object. The plot is also printed
#'   as a side effect when called interactively.
#'
#' @seealso \code{\link{btwar_fit}}, \code{\link{fitted.btwar}},
#'   \code{\link{residuals.btwar}}
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal theme labs
#'   element_blank scale_color_manual scale_linewidth_manual
#' @importFrom tidyr pivot_longer
#'
#' @method plot btwar
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- btwar_fit(y_tr_raw = train_series, y_te_raw = test_series)
#'
#' # Training set
#' plot(fit, dataset = "train")
#'
#' # Test set with observed overlay
#' plot(fit, dataset = "test", show_observed = TRUE)
#'
#' # Test set with an external benchmark
#' plot(fit, dataset = "test", external = arima_preds,
#'      external_name = "ARIMA")
#' }
plot.btwar <- function(x,
                       dataset       = c("train", "test"),
                       fs            = 1,
                       show_observed = TRUE,
                       external      = NULL,
                       external_name = "External",
                       ...) {

  # ---- input validation --------------------------------------------------
  if (!inherits(x, "btwar"))
    stop("'x' must be an object of class 'btwar'.")

  dataset <- match.arg(dataset)

  if (!is.numeric(fs) || length(fs) != 1L || fs <= 0)
    stop("'fs' must be a single positive number.")

  if (!is.logical(show_observed) || length(show_observed) != 1L)
    stop("'show_observed' must be a single logical value.")

  # ---- select dataset ----------------------------------------------------
  data_slot <- if (dataset == "train") x$train else x$test
  y_real    <- data_slot$y_real
  yhat      <- data_slot$yhat
  n         <- length(yhat)

  if (n == 0L)
    stop("The selected dataset is empty.")

  # ---- validate optional arguments ---------------------------------------
  if (show_observed && length(y_real) != n)
    stop("Observed and predicted series must have the same length.")

  if (!is.null(external)) {
    if (!is.numeric(external))
      stop("'external' must be a numeric vector.")
    if (length(external) != n)
      stop("'external' must have the same length as the selected dataset.")
    if (!is.character(external_name) || length(external_name) != 1L)
      stop("'external_name' must be a single character string.")
  }

  # ---- build data frame --------------------------------------------------
  time_axis <- seq.int(0L, n - 1L) / fs

  df        <- data.frame(time = time_axis, BTWAR = yhat)

  if (show_observed)
    df$Observed <- y_real

  if (!is.null(external))
    df[[external_name]] <- external

  # ---- reshape to long format --------------------------------------------
  df_long <- tidyr::pivot_longer(
    df,
    cols      = -time,
    names_to  = "Series",
    values_to = "Value"
  )

  # ---- colour and linewidth scheme ---------------------------------------
  series_names <- setdiff(names(df), "time")
  n_series     <- length(series_names)

  colours <- c(
    BTWAR    = "#2166AC",
    Observed = "#333333"
  )
  # fill remaining colours for any external series
  extra_cols <- grDevices::hcl.colors(
    max(1L, n_series - length(colours)), palette = "Dark 2"
  )
  extra_names <- setdiff(series_names, names(colours))
  colours[extra_names] <- extra_cols

  linewidths              <- stats::setNames(rep(0.7, n_series), series_names)
  linewidths["BTWAR"]     <- 0.9
  linewidths["Observed"]  <- 0.55

  # ---- build plot --------------------------------------------------------
  ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = time, y = Value, colour = Series, linewidth = Series)
  ) +
    ggplot2::geom_line(alpha = 0.9) +
    ggplot2::scale_color_manual(values = colours) +
    ggplot2::scale_linewidth_manual(values = linewidths) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position  = "top",
      legend.title     = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("BTWAR \u2014 ", dataset, " set predictions"),
      x     = "Time",
      y     = "Value"
    )
}
