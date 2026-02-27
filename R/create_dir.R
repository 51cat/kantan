#' Create Directory and Return Path
#'
#' @param path Character. Directory path to create.
#' @param recursive Logical. Whether to create parent directories if they don't exist. Default is TRUE.
#' @param absolute Logical. Whether to return absolute path. Default is TRUE.
#'
#' @return A character string of the created directory path.
#' @export
#'
#' @examples
#' create_dir("output")
#' create_dir("output/results", absolute = FALSE)
create_dir <- function(path, recursive = TRUE, absolute = TRUE) {
  if (!is.character(path) || length(path) != 1) {
    rlang::abort("`path` must be a single character string.")
  }

  if (nchar(path) == 0) {
    rlang::abort("`path` cannot be empty.")
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = recursive, showWarnings = FALSE)
  }

  if (absolute) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
  } else {
    path
  }
}
