#' Generate Sequential Color Palettes for Heatmaps
#'
#' @param n Integer. Number of colors to generate.
#' @param palette Character. Name of the color palette. Options include:
#'   "viridis", "plasma", "inferno", "magma", "cividis", "heat", "cool",
#'   "blues", "greens", "reds", "purples", "oranges", "greys".
#'   Default is "viridis".
#' @param alpha Numeric. Alpha transparency level (0-1). Default is 1.
#' @param reverse Logical. Whether to reverse the color order. Default is FALSE.
#' @param start Numeric. Starting point for color interpolation (0-1). Default is 0.
#' @param end Numeric. Ending point for color interpolation (0-1). Default is 1.
#'
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' seq_colors(10)
#' seq_colors(20, palette = "plasma")
#' seq_colors(10, palette = "blues", reverse = TRUE)
#' seq_colors(10, palette = "viridis", alpha = 0.7)
seq_colors <- function(n, palette = "viridis", alpha = 1, reverse = FALSE, start = 0, end = 1) {
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    rlang::abort("`n` must be a single positive integer.")
  }
  n <- as.integer(n)

  valid_palettes <- c(
    "viridis", "plasma", "inferno", "magma", "cividis",
    "heat", "cool", "blues", "greens", "reds", "purples", "oranges", "greys"
  )
  if (!palette %in% valid_palettes) {
    rlang::abort(paste("`palette` must be one of:", paste(valid_palettes, collapse = ", ")))
  }

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    rlang::abort("`alpha` must be a numeric value between 0 and 1.")
  }

  if (!is.numeric(start) || !is.numeric(end) || start < 0 || end > 1 || start >= end) {
    rlang::abort("`start` and `end` must be numeric values between 0 and 1, with start < end.")
  }

  palette_defs <- list(
    viridis = c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725"),
    plasma = c("#0d0887", "#6a00a8", "#b12a90", "#e16462", "#fca636", "#f0f921"),
    inferno = c("#000004", "#420a68", "#932667", "#dd513a", "#fca50a", "#fcffa4"),
    magma = c("#000004", "#3b0f70", "#8c2981", "#de4968", "#fe9f6d", "#fcfdbf"),
    cividis = c("#00204d", "#00306f", "#414287", "#706a8a", "#a09b92", "#fde725"),
    heat = c("#000004", "#490d56", "#8c2369", "#d34c6c", "#fe8772", "#fcfdbf"),
    cool = c("#ffffe5", "#f7fcb4", "#d9f0a3", "#addd8e", "#78c679", "#31a354", "#006837"),
    blues = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
    greens = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c"),
    reds = c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"),
    purples = c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f"),
    oranges = c("#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c", "#f16913", "#d94801"),
    greys = c("#ffffff", "#f0f0f0", "#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252")
  )

  base_colors <- palette_defs[[palette]]

  ramp_func <- grDevices::colorRampPalette(base_colors, alpha = TRUE)
  colors <- ramp_func(n)

  if (start != 0 || end != 1) {
    start_idx <- max(1, floor(start * (n - 1)) + 1)
    end_idx <- min(n, ceiling(end * (n - 1)) + 1)
    colors <- colors[start_idx:end_idx]
    if (length(colors) > n) {
      indices <- round(seq(1, length(colors), length.out = n))
      colors <- colors[indices]
    }
  }

  if (alpha < 1) {
    colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
  }

  if (reverse) {
    colors <- rev(colors)
  }

  colors
}
