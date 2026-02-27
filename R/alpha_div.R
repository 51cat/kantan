#' Calculate Alpha Diversity Indices
#'
#' @param x Numeric vector or matrix. Species abundances for a single sample (vector)
#'   or multiple samples (matrix with samples as rows and species as columns).
#' @param indices Character vector. Diversity indices to calculate. Options include:
#'   "shannon", "simpson", "invsimpson", "richness", "chao1", "ace", "fisher".
#'   Default is c("shannon", "simpson", "richness").
#' @param base Numeric. Base for logarithm in Shannon index. Default is 2.
#' @param taxon Character. Name of the taxonomic group for labeling. Default is NULL.
#'
#' @return A data frame with samples as rows and diversity indices as columns.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(10, 5, 3, 2, 1, 0, 0)
#' alpha_div(x)
#' alpha_div(x, indices = c("shannon", "chao1", "ace"))
#'
#' mat <- matrix(c(10, 5, 3, 2, 1, 0, 0,
#'                 8, 6, 4, 3, 2, 1, 1), nrow = 2, byrow = TRUE)
#' alpha_div(mat, indices = "all")
#' }
alpha_div <- function(x, indices = c("shannon", "simpson", "richness"), base = 2, taxon = NULL) {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    rlang::abort("Package 'vegan' is required. Install it with install.packages('vegan')")
  }

  if (is.vector(x)) {
    x <- matrix(x, nrow = 1)
  }

  if (!is.numeric(x)) {
    rlang::abort("`x` must be a numeric vector or matrix.")
  }

  if (any(x < 0, na.rm = TRUE)) {
    rlang::abort("`x` contains negative values. Abundances must be non-negative.")
  }

  x <- as.matrix(x)

  all_indices <- c("shannon", "simpson", "invsimpson", "richness", "chao1", "ace", "fisher")
  if (length(indices) == 1 && indices == "all") {
    indices <- all_indices
  }
  indices <- match.arg(indices, all_indices, several.ok = TRUE)

  n_samples <- nrow(x)
  result <- data.frame(sample = if (!is.null(rownames(x))) rownames(x) else seq_len(n_samples))

  if (!is.null(taxon)) {
    result$taxon <- taxon
  }

  if ("richness" %in% indices) {
    result$richness <- apply(x, 1, function(row) sum(row > 0))
  }

  if ("shannon" %in% indices) {
    result$shannon <- vegan::diversity(x, index = "shannon", base = base)
  }

  if ("simpson" %in% indices) {
    result$simpson <- vegan::diversity(x, index = "simpson")
  }

  if ("invsimpson" %in% indices) {
    result$invsimpson <- vegan::diversity(x, index = "invsimpson")
  }

  if ("chao1" %in% indices) {
    chao1_result <- vegan::estimateR(x)
    if (n_samples == 1) {
      result$chao1 <- chao1_result["S.chao1", ]
    } else {
      result$chao1 <- chao1_result["S.chao1", ]
    }
  }

  if ("ace" %in% indices) {
    ace_result <- vegan::estimateR(x)
    if (n_samples == 1) {
      result$ace <- ace_result["S.ACE", ]
    } else {
      result$ace <- ace_result["S.ACE", ]
    }
  }

  if ("fisher" %in% indices) {
    result$fisher <- apply(x, 1, function(row) {
      fit <- tryCatch(
        vegan::fisherfit(row),
        error = function(e) NULL
      )
      if (is.null(fit)) NA else fit$alpha
    })
  }

  result
}
