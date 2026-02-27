#' Display a Progress Bar
#'
#' @param current Integer. Current iteration number.
#' @param total Integer. Total number of iterations.
#' @param prefix Character. Text to display before the progress bar. Default is "".
#' @param suffix Character. Text to display after the progress bar. Default is "".
#' @param width Integer. Width of the progress bar in characters. Default is 50.
#' @param show_time Logical. Whether to show elapsed and remaining time. Default is TRUE.
#' @param clear Logical. Whether to clear the progress bar when complete. Default is TRUE.
#'
#' @return Invisible NULL. Called for side effect of displaying progress.
#' @export
#'
#' @examples
#' \dontrun{
#' for (i in 1:100) {
#'   Sys.sleep(0.02)
#'   progress_bar(i, 100, prefix = "Processing:")
#' }
#' }
progress_bar <- function(current, total, prefix = "", suffix = "", width = 50, show_time = TRUE, clear = TRUE) {
  if (!is.numeric(current) || !is.numeric(total)) {
    rlang::abort("`current` and `total` must be numeric.")
  }

  if (total <= 0) {
    rlang::abort("`total` must be a positive number.")
  }

  if (current < 0) {
    rlang::abort("`current` must be non-negative.")
  }

  current <- min(current, total)

  pct <- current / total
  filled <- round(width * pct)
  empty <- width - filled
  bar <- paste0(
    "\r", prefix, " [",
    paste(rep("=", filled), collapse = ""),
    if (filled < width) ">" else "",
    paste(rep(" ", empty), collapse = ""),
    "] ",
    sprintf("%6.2f", pct * 100), "%"
  )

  if (show_time) {
    if (!exists(".pb_start_time", envir = .GlobalEnv)) {
      assign(".pb_start_time", Sys.time(), envir = .GlobalEnv)
    }
    elapsed <- as.numeric(difftime(Sys.time(), .GlobalEnv$.pb_start_time, units = "secs"))
    if (current > 0 && current < total) {
      remaining <- elapsed / current * (total - current)
      bar <- paste0(bar, sprintf(" | ETA: %s", format_time(remaining)))
    } else if (current == total) {
      bar <- paste0(bar, sprintf(" | Elapsed: %s", format_time(elapsed)))
    }
  }

  bar <- paste0(bar, " ", suffix)

  cat(bar)

  if (current == total) {
    if (clear) {
      cat("\n")
    }
    if (exists(".pb_start_time", envir = .GlobalEnv)) {
      rm(".pb_start_time", envir = .GlobalEnv)
    }
  }

  invisible(NULL)
}

format_time <- function(seconds) {
  if (seconds < 60) {
    sprintf("%.0fs", seconds)
  } else if (seconds < 3600) {
    sprintf("%.0fm %.0fs", seconds %/% 60, seconds %% 60)
  } else {
    sprintf("%.0fh %.0fm", seconds %/% 3600, (seconds %% 3600) %/% 60)
  }
}
