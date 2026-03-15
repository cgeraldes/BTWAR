# ============================================================
# Plot Methods for BTWAR Objects
# ============================================================

# Suppress R CMD check NOTEs for ggplot2 non-standard evaluation variables
utils::globalVariables(c("time", "Series", "Value", "Re", "Im", "Legend", "x", "y"))


# ============================================================
# plot.btwar
# ============================================================

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
#'   (default), the fixed order Observed \eqn{\to} BTWAR \eqn{\to} external
#'   is used.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object. The plot is also printed
#'   as a side effect when called interactively.
#'
#' @seealso \code{\link{btwar_fit}}, \code{\link{plot_zpoles}},
#'   \code{\link{fitted.btwar}}, \code{\link{residuals.btwar}}
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal theme labs
#'   element_blank scale_color_manual scale_linewidth_manual
#'
#' @method plot btwar
#' @export
#'
\examples{
  \donttest{
    set.seed(42)
    y <- cumsum(rnorm(900))
    train_series <- y[1:600]
    test_series  <- y[601:900]

    fit <- btwar_fit(
      y_tr_raw = train_series,
      y_te_raw = test_series,
      fs       = 2,
      N_vec    = 2:5,
      As_vec   = c(20, 40),
      verbose  = FALSE
    )

    # Training set
    plot(fit, dataset = "train")

    # Test set with observed overlay
    plot(fit, dataset = "test", show_observed = TRUE)

    # Custom colours and line widths
    plot(fit,
         dataset         = "test",
         colour_btwar    = "steelblue",
         colour_observed = "black",
         lwd_btwar       = 1.2,
         lwd_observed    = 0.8)
  }
}
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
  if (length(extra_names) > 0L) {
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


# ============================================================
# plot_zpoles
# ============================================================

#' Plot Z-Plane Poles from a BTWAR Model
#'
#' Displays the poles of the selected BTW-AR model in the complex
#' Z-plane, together with the unit circle. Optionally overlays poles from
#' one or more external sources (e.g., a fitted ARIMA model or the true
#' data-generating poles).
#'
#' @param x Object of class \code{"btwar"}, as returned by
#'   \code{\link{btwar_fit}}. The selected model poles are read from
#'   \code{x$parameters$poles_z}.
#' @param external_list Named list of external pole sets to overlay.
#'   Each element must be a \code{data.frame} with columns \code{Re} and
#'   \code{Im} giving the real and imaginary parts of the poles. The
#'   element names are used as legend labels.
#'   Example: \code{list("ARIMA Z-Poles" = df_arima, "True Signal" = df_true)}.
#'   Default \code{NULL} (no external poles).
#' @param colour_selected Character string. Colour for the BTW-AR
#'   (selected) poles. Default \code{"green"}.
#' @param external_colours Character vector. Colours assigned to external
#'   pole sets in order. Recycled if fewer colours than sets are provided.
#'   Default \code{c("red", "darkgreen", "purple", "orange", "brown", "navy")}.
#' @param external_shapes Integer vector. \code{\link[ggplot2]{shape}} codes
#'   assigned to external pole sets in order. Recycled if needed.
#'   Default \code{c(17L, 15L, 18L, 8L, 10L, 12L)}.
#' @param lim Positive numeric. Half-width of the plot window on both axes.
#'   Default \code{1.5}.
#' @param base_size Positive numeric. Base font size passed to
#'   \code{\link[ggplot2]{theme_minimal}}. Default \code{10}.
#' @param title Character string. Plot title. If \code{NULL} (default), no
#'   title is added.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object. The plot is also printed
#'   as a side effect when called interactively.
#'
#' @details
#' Poles of the selected BTW-AR model are extracted from
#' \code{x$parameters$poles_z}, which is populated by \code{\link{btwar_fit}}
#' via the internal cross-validation routine. The unit circle is drawn as a
#' reference: poles inside the circle correspond to stable filters.
#'
#' Colours and shapes for external pole sets are assigned sequentially from
#' \code{external_colours} and \code{external_shapes}, with circular
#' recycling when the number of sets exceeds the palette length.
#'
#' @seealso \code{\link{btwar_fit}}, \code{\link{plot.btwar}}
#'
#' @importFrom ggplot2 ggplot aes geom_path geom_hline geom_vline geom_point
#'   coord_fixed xlim ylim labs scale_color_manual scale_shape_manual
#'   theme_minimal theme
#'
#' @export
#'
#' @examples
#' \donttest{
#' fit <- btwar_fit(y_tr_raw = train_series, y_te_raw = test_series, fs = 12000)
#'
#' # BTW-AR selected poles only
#' plot_zpoles(fit)
#'
#' # Overlay ARIMA poles
#' phi_arima <- coef(fit_arima)[grep("^ar", names(coef(fit_arima)))]
#' df_arima  <- data.frame(Re = Re(polyroot(c(1, -phi_arima))),
#'                         Im = Im(polyroot(c(1, -phi_arima))))
#' plot_zpoles(fit, external_list = list("ARIMA Z-Poles" = df_arima))
#'
#' # Overlay multiple external pole sets
#' plot_zpoles(
#'   fit,
#'   external_list = list(
#'     "ARIMA Z-Poles"       = df_arima,
#'     "True Signal Z-Poles" = df_true
#'   )
#' )
#' }
plot_zpoles <- function(x,
                        external_list    = NULL,
                        colour_selected  = "green",
                        external_colours = c("red", "darkgreen", "purple",
                                             "orange", "brown", "navy"),
                        external_shapes  = c(17L, 15L, 18L, 8L, 10L, 12L),
                        lim              = 1.5,
                        base_size        = 10,
                        title            = NULL) {

  # ---- input validation --------------------------------------------------
  if (!inherits(x, "btwar"))
    stop("'x' must be an object of class 'btwar'.")
  if (is.null(x$parameters$poles_z))
    stop(paste0(
      "'x$parameters$poles_z' is NULL. ",
      "Re-fit the model with a version of btwar_fit() that exposes poles_z."
    ))
  if (!is.null(external_list)) {
    if (!is.list(external_list) || is.null(names(external_list)) ||
        any(nchar(names(external_list)) == 0L))
      stop("'external_list' must be a non-empty named list.")
    for (nm in names(external_list)) {
      df <- external_list[[nm]]
      if (!is.data.frame(df) || !all(c("Re", "Im") %in% names(df)))
        stop(sprintf(
          "Each element of 'external_list' must be a data.frame with columns 'Re' and 'Im'. Problem with element '%s'.",
          nm
        ))
    }
  }
  if (!is.numeric(lim) || length(lim) != 1L || lim <= 0)
    stop("'lim' must be a single positive number.")
  if (!is.numeric(base_size) || length(base_size) != 1L || base_size <= 0)
    stop("'base_size' must be a single positive number.")
  if (!is.null(title) && (!is.character(title) || length(title) != 1L))
    stop("'title' must be a single character string or NULL.")

  # ---- unit circle -------------------------------------------------------
  theta     <- seq(0, 2 * pi, length.out = 300L)
  df_circle <- data.frame(x = cos(theta), y = sin(theta))

  # ---- BTW-AR (selected) poles -------------------------------------------
  poles_z <- x$parameters$poles_z
  df_btw  <- data.frame(Re = Re(poles_z), Im = Im(poles_z))
  df_btw$Legend <- "BTW-AR (selected)"
  df_all <- df_btw

  color_vals <- c("BTW-AR (selected)" = colour_selected)
  shape_vals <- c("BTW-AR (selected)" = 16L)

  # ---- external pole sets ------------------------------------------------
  if (!is.null(external_list)) {
    n_ext <- length(external_list)
    for (i in seq_len(n_ext)) {
      lbl             <- names(external_list)[i]
      df_ext          <- external_list[[i]]
      df_ext$Legend   <- lbl
      df_all          <- rbind(df_all, df_ext)
      color_vals[lbl] <- external_colours[(i - 1L) %% length(external_colours) + 1L]
      shape_vals[lbl] <- external_shapes[(i - 1L)  %% length(external_shapes)  + 1L]
    }
  }

  # Factor levels fix legend and layer order
  level_order   <- c("BTW-AR (selected)", names(external_list))
  df_all$Legend <- factor(df_all$Legend, levels = level_order)

  # ---- build plot --------------------------------------------------------
  p <- ggplot2::ggplot(
    df_all,
    ggplot2::aes(x = Re, y = Im, color = Legend, shape = Legend)
  ) +
    ggplot2::geom_path(
      data        = df_circle,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE,
      color       = "black",
      linewidth   = 0.8
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(size = 3, alpha = 0.9) +
    ggplot2::coord_fixed() +
    ggplot2::xlim(-lim, lim) +
    ggplot2::ylim(-lim, lim) +
    ggplot2::labs(
      title = title,
      x     = "Re{z}",
      y     = "Im{z}",
      color = NULL,
      shape = NULL
    ) +
    ggplot2::scale_color_manual(values = color_vals, drop = FALSE) +
    ggplot2::scale_shape_manual(values = shape_vals, drop = FALSE) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "top")

  p
}

# ============================================================
# Bode Magnitude Plot for BTWAR Objects
# ============================================================


#' Plot the Bode Magnitude Diagram of a BTWAR Model
#'
#' Draws the Butterworth filter magnitude response (in dB) associated with
#' the selected BTW-AR model, using the optimal order \eqn{N} and
#' cutoff frequency \eqn{f_c} stored in the fitted object. Reference lines
#' are drawn at the cutoff frequency, the Nyquist frequency, and their
#' corresponding magnitude levels.
#'
#' @param x Object of class \code{"btwar"}, as returned by
#'   \code{\link{btwar_fit}}. The parameters \code{N_opt}, \code{A_opt},
#'   and \code{fc_opt} are read from \code{x$parameters}.
#' @param fs Positive numeric. Sampling frequency (Hz) used to derive the
#'   Nyquist frequency as \eqn{f_s / 2}. Must match the value passed to
#'   \code{\link{btwar_fit}}.
#' @param n_freq Positive integer. Number of frequency points used to
#'   evaluate the magnitude response. Default \code{2000L}.
#' @param colour_magnitude Character string. Colour for the magnitude
#'   response curve. Default \code{"blue"}.
#' @param colour_cutoff Character string. Colour for the cutoff frequency
#'   reference lines (vertical dashed and horizontal dotted).
#'   Default \code{"orange"}.
#' @param colour_nyquist Character string. Colour for the Nyquist frequency
#'   reference lines (vertical dashed and horizontal dotted).
#'   Default \code{"red"}.
#' @param lwd_magnitude Positive numeric. Line width for the magnitude
#'   response curve. Default \code{1.2}.
#' @param lwd_ref Positive numeric. Line width for all reference lines.
#'   Default \code{1.0}.
#' @param base_size Positive numeric. Base font size passed to
#'   \code{\link[ggplot2]{theme_minimal}}. Default \code{11}.
#' @param title Character string. Plot title. If \code{NULL} (default), a
#'   title is generated automatically from the fitted model parameters.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object. The plot is also printed
#'   as a side effect when called interactively.
#'
#' @details
#' The Butterworth filter magnitude response is
#' \deqn{|H(f)| = \frac{1}{\sqrt{1 + \beta^2 \, (2\pi f)^{2N}}},
#'       \quad \beta = \frac{1}{(2\pi f_c)^N},}
#' evaluated on a log-spaced frequency axis from \eqn{10^{-3}} Hz to the
#' Nyquist frequency. The magnitude at the cutoff frequency equals
#' \eqn{-3} dB (\eqn{1/\sqrt{2}}), and the magnitude at the Nyquist
#' frequency reflects the actual stopband attenuation achieved by the
#' selected filter.
#'
#' @seealso \code{\link{btwar_fit}}, \code{\link{plot.btwar}},
#'   \code{\link{plot_zpoles}}
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_vline geom_hline
#'   scale_x_log10 scale_color_manual labs theme_minimal theme
#'   element_blank
#' @importFrom scales label_math
#'
#' @export
#'
#' @examples
#' \donttest{
#' fit <- btwar_fit(
#'   y_tr_raw = train_series,
#'   y_te_raw = test_series,
#'   fs       = 12000
#' )
#'
#' # Default Bode plot
#' plot_bode(fit, fs = 12000)
#'
#' # Custom colours
#' plot_bode(fit, fs = 12000,
#'           colour_magnitude = "steelblue",
#'           colour_cutoff    = "darkorange",
#'           colour_nyquist   = "firebrick")
#' }
plot_bode <- function(x,
                      fs               = 2,
                      n_freq           = 2000L,
                      colour_magnitude = "blue",
                      colour_cutoff    = "orange",
                      colour_nyquist   = "red",
                      lwd_magnitude    = 1.2,
                      lwd_ref          = 1.0,
                      base_size        = 11,
                      title            = NULL) {
  .x <- NULL
  # ---- input validation --------------------------------------------------
  if (!inherits(x, "btwar"))
    stop("'x' must be an object of class 'btwar'.")
  if (!is.numeric(fs) || length(fs) != 1L || fs <= 0)
    stop("'fs' must be a single positive number.")
  if (!is.numeric(n_freq) || length(n_freq) != 1L || n_freq < 2L)
    stop("'n_freq' must be a single integer >= 2.")
  if (!is.numeric(lwd_magnitude) || length(lwd_magnitude) != 1L || lwd_magnitude <= 0)
    stop("'lwd_magnitude' must be a single positive number.")
  if (!is.numeric(lwd_ref) || length(lwd_ref) != 1L || lwd_ref <= 0)
    stop("'lwd_ref' must be a single positive number.")
  if (!is.numeric(base_size) || length(base_size) != 1L || base_size <= 0)
    stop("'base_size' must be a single positive number.")
  if (!is.null(title) && (!is.character(title) || length(title) != 1L))
    stop("'title' must be a single character string or NULL.")

  # ---- extract model parameters ------------------------------------------
  N  <- x$parameters$N_opt
  fc <- x$parameters$fc_opt
  A  <- x$parameters$A_opt

  if (is.null(N) || is.null(fc) || is.null(A))
    stop(paste0(
      "One or more of 'N_opt', 'fc_opt', 'A_opt' is NULL in 'x$parameters'. ",
      "Re-fit the model with a current version of btwar_fit()."
    ))

  # ---- frequency grid ----------------------------------------------------
  fNyquist <- fs / 2
  f        <- seq(1e-3, fNyquist, length.out = as.integer(n_freq))

  # ---- Butterworth magnitude response ------------------------------------
  beta  <- 1 / (2 * pi * fc)^N
  Omega <- 2 * pi * f
  H     <- 1 / sqrt(1 + beta^2 * Omega^(2 * N))
  H_dB  <- 20 * log10(H)

  df_bode <- data.frame(f = f, H_dB = H_dB)

  # ---- reference levels --------------------------------------------------
  Hc_dB <- 20 * log10(1 / sqrt(2))   # -3 dB at cutoff (exact by definition)
  Hs_dB <- 20 * log10(
    1 / sqrt(1 + beta^2 * (2 * pi * fNyquist)^(2 * N))
  )

  # ---- legend labels with parameter values --------------------------------
  lbl_mag     <- "Magnitude response"
  lbl_cutoff  <- sprintf("Cutoff frequency (%.1f Hz)", fc)
  lbl_nyquist <- sprintf("Nyquist frequency (%.1f Hz)", fNyquist)
  lbl_Hc      <- sprintf("Magnitude at cutoff (%.1f dB)", Hc_dB)
  lbl_Hs      <- sprintf("Magnitude at Nyquist (%.1f dB)", Hs_dB)

  colour_vals <- stats::setNames(
    c(colour_magnitude, colour_cutoff, colour_nyquist,
      colour_cutoff,    colour_nyquist),
    c(lbl_mag, lbl_cutoff, lbl_nyquist, lbl_Hc, lbl_Hs)
  )

  # ---- title -------------------------------------------------------------
  plot_title <- if (!is.null(title)) {
    title
  } else {
    sprintf(
      "Butterworth Magnitude Response \u2014 N = %d, fc = %.1f Hz, A = %.0f dB",
      N, fc, A
    )
  }

  # ---- build plot --------------------------------------------------------
  ggplot2::ggplot(df_bode, ggplot2::aes(x = f, y = H_dB)) +

    ggplot2::geom_line(
      ggplot2::aes(color = lbl_mag),
      linewidth = lwd_magnitude
    ) +

    ggplot2::geom_vline(
      ggplot2::aes(xintercept = fc, color = lbl_cutoff),
      linetype  = "dashed",
      linewidth = lwd_ref
    ) +

    ggplot2::geom_vline(
      ggplot2::aes(xintercept = fNyquist, color = lbl_nyquist),
      linetype  = "dashed",
      linewidth = lwd_ref
    ) +

    ggplot2::geom_hline(
      ggplot2::aes(yintercept = Hc_dB, color = lbl_Hc),
      linetype  = "dotted",
      linewidth = lwd_ref
    ) +

    ggplot2::geom_hline(
      ggplot2::aes(yintercept = Hs_dB, color = lbl_Hs),
      linetype  = "dotted",
      linewidth = lwd_ref
    ) +

    ggplot2::scale_x_log10(
      labels = scales::label_math(10^.x)
    ) +

    ggplot2::scale_color_manual(values = colour_vals) +

    ggplot2::labs(
      title = plot_title,
      x     = expression("Frequency " * f * " (Hz)"),
      y     = "Magnitude (dB)",
      color = NULL
    ) +

    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}
