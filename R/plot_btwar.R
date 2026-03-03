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
#' Series are drawn in the following fixed order (back to front):
#' Observed, BTWAR, external.
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
#' @param title Character string. Plot title. If \code{NULL} (default), a
#'   title is generated automatically from the dataset name.
#' @param colour_btwar Character string. Colour for the BTWAR prediction
#'   line. Default \code{"#2166AC"}.
#' @param colour_observed Character string. Colour for the observed series
#'   line. Default \code{"#333333"}.
#' @param colour_external Character string. Colour for the external series
#'   line. If \code{NULL} (default), a colour is assigned automatically
#'   from the \code{palette}.
#' @param lwd_btwar Positive numeric. Line width for the BTWAR prediction
#'   line. Default \code{0.9}.
#' @param lwd_observed Positive numeric. Line width for the observed series
#'   line. Default \code{0.55}.
#' @param lwd_external Positive numeric. Line width for any external series
#'   line. Default \code{0.7}.
#' @param alpha Numeric in \eqn{(0, 1]}. Transparency of all lines.
#'   Default \code{0.9}.
#' @param base_size Positive numeric. Base font size passed to
#'   \code{\link[ggplot2]{theme_minimal}}. Default \code{11}.
#' @param palette Character string. Name of the \code{\link[grDevices]{hcl.colors}}
#'   palette used for additional external series when \code{colour_external}
#'   is \code{NULL}. Default \code{"Dark 2"}.
#' @param layer_order Character vector. Controls the drawing order (back to
#'   front) and legend order of the series. Must contain the names of all
#'   active series: \code{"Observed"}, \code{"BTWAR"}, and the value of
#'   \code{external_name} if an external series is supplied. If \code{NULL}
#'   (default), the fixed order Observed → BTWAR → external is used.
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
#' # Custom colours and line widths
#' plot(fit,
#'      dataset         = "test",
#'      colour_btwar    = "steelblue",
#'      colour_observed = "black",
#'      lwd_btwar       = 1.2,
#'      lwd_observed    = 0.8)
#'
#' # Test set with an external benchmark in red
#' plot(fit,
#'      dataset         = "test",
#'      external        = arima_preds,
#'      external_name   = "ARIMA",
#'      colour_external = "red")
#' }
plot.btwar <- function(x,
                       dataset         = c("train", "test"),
                       fs              = 1,
                       show_observed   = TRUE,
                       external        = NULL,
                       external_name   = "External",
                       title           = NULL,
                       colour_btwar    = "#2166AC",
                       colour_observed = "#333333",
                       colour_external = NULL,
                       lwd_btwar       = 0.9,
                       lwd_observed    = 0.55,
                       lwd_external    = 0.7,
                       alpha           = 0.9,
                       base_size       = 11,
                       palette         = "Dark 2",
                       layer_order     = NULL,
                       ...) {

  # ---- input validation --------------------------------------------------
  if (!inherits(x, "btwar"))
    stop("'x' must be an object of class 'btwar'.")
  dataset <- match.arg(dataset)
  if (!is.numeric(fs) || length(fs) != 1L || fs <= 0)
    stop("'fs' must be a single positive number.")
  if (!is.logical(show_observed) || length(show_observed) != 1L)
    stop("'show_observed' must be a single logical value.")
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha > 1)
    stop("'alpha' must be a single numeric value in (0, 1].")
  if (!is.numeric(base_size) || length(base_size) != 1L || base_size <= 0)
    stop("'base_size' must be a single positive number.")
  if (!is.null(title) && (!is.character(title) || length(title) != 1L))
    stop("'title' must be a single character string or NULL.")
  if (!is.null(colour_external) &&
      (!is.character(colour_external) || length(colour_external) != 1L))
    stop("'colour_external' must be a single character string or NULL.")

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

  # ---- build data frame in fixed layer order: Observed, BTWAR, external --
  time_axis <- seq.int(0L, n - 1L) / fs

  df_list <- list()

  if (show_observed)
    df_list[["Observed"]] <- data.frame(
      time = time_axis, Series = "Observed", Value = y_real
    )

  df_list[["BTWAR"]] <- data.frame(
    time = time_axis, Series = "BTWAR", Value = yhat
  )

  if (!is.null(external))
    df_list[[external_name]] <- data.frame(
      time = time_axis, Series = external_name, Value = external
    )

  df_long <- do.call(rbind, df_list)

  # Factor levels control both draw order and legend order
  active_series <- names(df_list)
  if (!is.null(layer_order)) {
    if (!is.character(layer_order))
      stop("'layer_order' must be a character vector.")
    if (!all(active_series %in% layer_order) ||
        !all(layer_order %in% active_series))
      stop(paste0(
        "'layer_order' must contain exactly these series: ",
        paste(active_series, collapse = ", ")
      ))
    active_series <- layer_order
  }
  df_long$Series <- factor(df_long$Series, levels = active_series)

  # ---- colour and linewidth scheme ---------------------------------------
  series_names <- active_series
  n_series     <- length(series_names)

  colours <- c(
    Observed = colour_observed,
    BTWAR    = colour_btwar
  )

  extra_names <- setdiff(series_names, names(colours))
  if (length(extra_names) > 0) {
    if (!is.null(colour_external)) {
      colours[extra_names] <- colour_external
    } else {
      extra_cols <- grDevices::hcl.colors(
        max(1L, length(extra_names)), palette = palette
      )
      colours[extra_names] <- extra_cols
    }
  }

  linewidths             <- stats::setNames(rep(lwd_external, n_series), series_names)
  linewidths["BTWAR"]    <- lwd_btwar
  linewidths["Observed"] <- lwd_observed

  # ---- title -------------------------------------------------------------
  plot_title <- if (!is.null(title)) {
    title
  } else {
    paste0("BTWAR \u2014 ", dataset, " set predictions")
  }

  # ---- build plot --------------------------------------------------------
  ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = time, y = Value, colour = Series, linewidth = Series)
  ) +
    ggplot2::geom_line(alpha = alpha) +
    ggplot2::scale_color_manual(values = colours) +
    ggplot2::scale_linewidth_manual(values = linewidths) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "top",
      legend.title     = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = plot_title,
      x     = "Time",
      y     = "Value"
    )
}
