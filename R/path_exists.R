#' Check if Path Exists
#'
#' @param path Character. Path to check.
#' @param type Character. Expected type: "any", "file", or "dir". Default is "any".
#' @param error Logical. If TRUE, throw error when path doesn't exist. Default is FALSE.
#'
#' @return Logical. TRUE if path exists and matches type, FALSE otherwise. 
#'   If error = TRUE, throws error instead of returning FALSE.
#' @export
#'
#' @examples
#' path_exists(tempdir())
#' path_exists(tempfile(), type = "file")
#' path_exists("nonexistent", error = FALSE)
path_exists <- function(path, type = "any", error = FALSE) {
  if (!is.character(path) || length(path) != 1) {
    rlang::abort("`path` must be a single character string.")
  }

  type <- match.arg(type, c("any", "file", "dir"))

  exists <- switch(type,
    any = file.exists(path),
    file = file.exists(path) && !dir.exists(path),
    dir = dir.exists(path)
  )

  if (!exists && error) {
    msg <- switch(type,
      any = paste0("Path does not exist: ", path),
      file = paste0("File does not exist: ", path),
      dir = paste0("Directory does not exist: ", path)
    )
    rlang::abort(msg)
  }

  exists
}
