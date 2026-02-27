#' Exploratory Data Analysis
#'
#' Perform comprehensive exploratory data analysis on a data frame.
#'
#' @param df A data frame to analyze.
#' @param outlier_method Character. Method for outlier detection: "iqr" or "zscore". Default is "iqr".
#' @param zscore_threshold Numeric. Threshold for z-score outlier detection. Default is 3.
#' @param include_corr Logical. Whether to compute correlation matrix for numeric columns. Default is TRUE.
#' @param max_unique Integer. Maximum unique values for a column to be considered categorical. Default is 10.
#'
#' @return A list containing:
#' \describe{
#'   \item{dimensions}{Data frame dimensions (rows, columns)}
#'   \item{column_types}{Column type summary}
#'   \item{memory_usage}{Estimated memory usage in bytes}
#'   \item{missing_values}{Missing value counts and percentages per column}
#'   \item{numeric_summary}{Summary statistics for numeric columns}
#'   \item{categorical_summary}{Frequency tables for categorical columns}
#'   \item{outliers}{Outlier counts per numeric column}
#'   \item{correlation}{Correlation matrix for numeric columns (if enabled)}
#'   \item{insights}{Automated insights and recommendations}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' result <- eda(iris)
#' result$dimensions
#' result$missing_values
#' result$numeric_summary
#' }
eda <- function(df, outlier_method = c("iqr", "zscore"), zscore_threshold = 3,
                include_corr = TRUE, max_unique = 10) {
  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data frame.")
  }

  outlier_method <- match.arg(outlier_method)

  if (!is.numeric(zscore_threshold) || zscore_threshold <= 0) {
    rlang::abort("`zscore_threshold` must be a positive numeric value.")
  }

  if (!is.logical(include_corr)) {
    rlang::abort("`include_corr` must be TRUE or FALSE.")
  }

  if (!is.numeric(max_unique) || max_unique < 1) {
    rlang::abort("`max_unique` must be a positive integer.")
  }

  result <- list()
  insights <- character()

  result$dimensions <- c(rows = nrow(df), columns = ncol(df))

  col_types <- vapply(df, function(x) {
    if (is.factor(x)) "factor"
    else if (is.character(x)) "character"
    else if (is.numeric(x)) {
      if (is.integer(x)) "integer"
      else "double"
    }
    else if (is.logical(x)) "logical"
    else if (inherits(x, "Date")) "Date"
    else if (inherits(x, "POSIXt")) "POSIXt"
    else "other"
  }, character(1))

  result$column_types <- as.list(table(col_types))

  result$memory_usage <- sum(utils::object.size(df))

  na_counts <- vapply(df, function(x) sum(is.na(x)), integer(1))
  na_pcts <- round(na_counts / nrow(df) * 100, 2)
  result$missing_values <- data.frame(
    column = names(df),
    na_count = na_counts,
    na_percent = na_pcts,
    stringsAsFactors = FALSE
  )
  rownames(result$missing_values) <- NULL

  high_na <- na_pcts[na_pcts > 20]
  if (length(high_na) > 0) {
    insights <- c(insights, paste0(
      "High missing rate: ",
      paste(names(high_na), "(", high_na, "%)", collapse = ", ")
    ))
  }

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- numeric_cols[!vapply(df[numeric_cols], function(x) {
    length(unique(x[!is.na(x)])) <= max_unique
  }, logical(1))]

  if (length(numeric_cols) > 0) {
    num_summary <- lapply(df[numeric_cols], function(x) {
      valid_x <- x[!is.na(x)]
      if (length(valid_x) == 0) {
        return(c(n = 0, mean = NA_real_, sd = NA_real_, min = NA_real_,
                 q1 = NA_real_, median = NA_real_, q3 = NA_real_,
                 max = NA_real_, skewness = NA_real_, kurtosis = NA_real_))
      }
      n <- length(valid_x)
      mn <- mean(valid_x)
      s <- stats::sd(valid_x)
      quants <- stats::quantile(valid_x, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
      skewness <- sum((valid_x - mn)^3) / (n * s^3)
      kurtosis <- sum((valid_x - mn)^4) / (n * s^4) - 3
      c(n = n, mean = mn, sd = s, min = quants[1],
        q1 = quants[2], median = quants[3], q3 = quants[4],
        max = quants[5], skewness = skewness, kurtosis = kurtosis)
    })
    result$numeric_summary <- as.data.frame(
      do.call(rbind, num_summary),
      stringsAsFactors = FALSE
    )
    result$numeric_summary$column <- rownames(result$numeric_summary)
    rownames(result$numeric_summary) <- NULL
    result$numeric_summary <- result$numeric_summary[, c("column", "n", "mean", "sd",
                                                          "min", "q1", "median", "q3", "max",
                                                          "skewness", "kurtosis")]

    detect_outliers <- function(x, method, threshold) {
      valid_x <- x[!is.na(x)]
      if (length(valid_x) == 0) return(0)

      if (method == "iqr") {
        q <- stats::quantile(valid_x, probs = c(0.25, 0.75))
        iqr_val <- q[2] - q[1]
        lower <- q[1] - 1.5 * iqr_val
        upper <- q[2] + 1.5 * iqr_val
        sum(valid_x < lower | valid_x > upper)
      } else {
        z <- (valid_x - mean(valid_x)) / stats::sd(valid_x)
        sum(abs(z) > threshold)
      }
    }

    outlier_counts <- vapply(df[numeric_cols], detect_outliers,
                             integer(1), method = outlier_method,
                             threshold = zscore_threshold)
    outlier_pcts <- round(outlier_counts / nrow(df) * 100, 2)
    result$outliers <- data.frame(
      column = numeric_cols,
      outlier_count = outlier_counts,
      outlier_percent = outlier_pcts,
      stringsAsFactors = FALSE
    )

    high_outlier <- outlier_pcts[outlier_pcts > 5]
    if (length(high_outlier) > 0) {
      insights <- c(insights, paste0(
        "High outlier rate: ",
        paste(names(high_outlier), "(", high_outlier, "%)", collapse = ", ")
      ))
    }
  } else {
    result$numeric_summary <- NULL
    result$outliers <- NULL
  }

  cat_cols <- names(df)[vapply(df, function(x) {
    is.factor(x) || is.character(x) ||
      (is.numeric(x) && length(unique(x[!is.na(x)])) <= max_unique)
  }, logical(1))]

  if (length(cat_cols) > 0) {
    cat_summary <- lapply(df[cat_cols], function(x) {
      freq <- table(x, useNA = "ifany")
      prop <- prop.table(freq) * 100
      df_out <- data.frame(
        value = names(freq),
        count = as.integer(freq),
        percent = round(as.numeric(prop), 2),
        stringsAsFactors = FALSE
      )
      df_out[order(df_out$count, decreasing = TRUE), ]
    })
    names(cat_summary) <- cat_cols
    result$categorical_summary <- cat_summary

    for (col in cat_cols) {
      top_prop <- max(cat_summary[[col]]$percent, na.rm = TRUE)
      if (top_prop > 80) {
        insights <- c(insights, paste0(
          "Imbalanced categorical: ", col, " (top category ", round(top_prop, 1), "%)"
        ))
      }
    }
  } else {
    result$categorical_summary <- NULL
  }

  if (include_corr && length(numeric_cols) >= 2) {
    valid_numeric <- df[numeric_cols]
    complete_rows <- complete.cases(valid_numeric)

    if (sum(complete_rows) >= 3) {
      result$correlation <- stats::cor(valid_numeric[complete_rows, , drop = FALSE])
      high_corr <- which(abs(result$correlation) > 0.8 & result$correlation != 1, arr.ind = TRUE)
      if (nrow(high_corr) > 0) {
        corr_pairs <- apply(high_corr, 1, function(idx) {
          paste(rownames(result$correlation)[idx[1]],
                rownames(result$correlation)[idx[2]], sep = " - ")
        })
        insights <- c(insights, paste0(
          "High correlations: ", paste(unique(corr_pairs), collapse = ", ")
        ))
      }
    } else {
      result$correlation <- NULL
      insights <- c(insights, "Correlation not computed: too many missing values")
    }
  } else {
    result$correlation <- NULL
  }

  dup_rows <- sum(duplicated(df))
  if (dup_rows > 0) {
    insights <- c(insights, paste0("Duplicate rows: ", dup_rows))
  }

  if (nrow(df) < 100) {
    insights <- c(insights, "Small dataset (< 100 rows): consider non-parametric methods")
  }

  result$insights <- if (length(insights) > 0) insights else "No significant issues detected"

  result
}
