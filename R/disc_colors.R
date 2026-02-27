#' Generate Colors for Categorical Vectors
#'
#' @param x A categorical vector (factor or character).
#' @param palette Character. Name of the color palette. Options include:
#'   "set1", "set2", "set3", "paired", "dark2", "accent", "pastel1", "pastel2",
#'   "category10", "tableau", "okabeIto". Default is "set1".
#' @param alpha Numeric. Alpha transparency level (0-1). Default is 1.
#' @param shuffle Logical. Whether to shuffle color order. Default is FALSE.
#' @param seed Integer. Random seed for reproducible shuffling. Default is NULL.
#' @param named Logical. Whether to return named vector with category names. Default is FALSE.
#'
#' @return A character vector of hex color codes with the same length as `x`.
#' @export
#'
#' @examples
#' x <- c("A", "B", "C", "A", "B")
#' disc_colors(x)
#' disc_colors(x, palette = "dark2")
#' disc_colors(iris$Species, palette = "tableau")
disc_colors <- function(x, palette = "set1", alpha = 1, shuffle = FALSE, seed = NULL, named = FALSE) {
  if (!is.factor(x) && !is.character(x)) {
    rlang::abort("`x` must be a factor or character vector.")
  }

  valid_palettes <- c(
    "set1", "set2", "set3", "paired", "dark2", "accent",
    "pastel1", "pastel2", "category10", "tableau", "okabeIto"
  )
  if (!palette %in% valid_palettes) {
    rlang::abort(paste("`palette` must be one of:", paste(valid_palettes, collapse = ", ")))
  }

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    rlang::abort("`alpha` must be a numeric value between 0 and 1.")
  }

  levels_x <- if (is.factor(x)) levels(x) else unique(x)
  n_levels <- length(levels_x)

  palette_defs <- list(
    set1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
    set2 = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"),
    set3 = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"),
    paired = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"),
    dark2 = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
    accent = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666"),
    pastel1 = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2"),
    pastel2 = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC"),
    category10 = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
    tableau = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"),
    okabeIto = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  )

  base_colors <- palette_defs[[palette]]

  if (n_levels > length(base_colors)) {
    base_colors <- grDevices::colorRampPalette(base_colors)(n_levels)
  }

  if (shuffle) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    base_colors <- sample(base_colors, n_levels)
  }

  if (alpha < 1) {
    base_colors <- grDevices::adjustcolor(base_colors[1:n_levels], alpha.f = alpha)
  }

  names(base_colors) <- levels_x
  colors <- base_colors[as.character(x)]

  if (named) {
    colors
  } else {
    unname(colors)
  }
}
