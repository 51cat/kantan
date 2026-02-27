#' Save ggplot Object to File
#'
#' @param plot A ggplot object. If NULL, saves the last plot.
#' @param filename Character. Output filename with extension.
#' @param width Numeric. Width in inches. Default is 10.
#' @param height Numeric. Height in inches. Default is 8.
#' @param dpi Numeric. Resolution in dots per inch. Default is 300.
#' @param format Character. Output format: "pdf", "png", "svg", "tiff", "jpeg", "eps", "ps". 
#'   If NULL, inferred from filename extension. Default is NULL.
#' @param scale Numeric. Scaling factor. Default is 1.
#' @param limitsize Logical. Whether to limit image size to 50x50 inches. Default is FALSE.
#' @param bg Character. Background color. Default is "white".
#' @param device Graphics device function. If NULL, inferred from format. Default is NULL.
#' @param path Character. Directory path to save file. If NULL, uses current directory. Default is NULL.
#' @param create_dir Logical. Whether to create directory if it doesn't exist. Default is TRUE.
#' @param verbose Logical. Whether to print save message. Default is TRUE.
#'
#' @return Invisibly returns the full path to the saved file.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' savegg(p, "plot.png")
#' savegg(p, "output/plot.pdf", width = 12, height = 6)
#' savegg(p, "plot.png", dpi = 600, scale = 1.5)
#' savegg(filename = "last_plot.png")  # saves last plot
#' }
savegg <- function(plot = NULL, filename, width = 10, height = 8, dpi = 300,
                   format = NULL, scale = 1, limitsize = FALSE, bg = "white",
                   device = NULL, path = NULL, create_dir = TRUE, verbose = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required. Install it with install.packages('ggplot2')")
  }

  if (!is.character(filename) || length(filename) != 1) {
    rlang::abort("`filename` must be a single character string.")
  }

  if (!is.null(plot) && !inherits(plot, "ggplot")) {
    rlang::abort("`plot` must be a ggplot object.")
  }

  if (!is.numeric(width) || width <= 0) {
    rlang::abort("`width` must be a positive number.")
  }

  if (!is.numeric(height) || height <= 0) {
    rlang::abort("`height` must be a positive number.")
  }

  if (!is.numeric(dpi) || dpi <= 0) {
    rlang::abort("`dpi` must be a positive number.")
  }

  valid_formats <- c("pdf", "png", "svg", "tiff", "jpeg", "eps", "ps", "jpg")
  if (!is.null(format) && !format %in% valid_formats) {
    rlang::abort(paste("`format` must be one of:", paste(valid_formats, collapse = ", ")))
  }

  if (!is.null(path) && create_dir) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }

  full_path <- if (!is.null(path)) file.path(path, filename) else filename

  ext <- tolower(tools::file_ext(filename))
  if (ext == "jpg") ext <- "jpeg"

  format <- if (!is.null(format)) format else ext
  if (nchar(ext) == 0 && is.null(format)) {
    rlang::abort("Cannot infer format from filename. Please specify `format` or include extension in filename.")
  }

  device_func <- device
  if (is.null(device_func)) {
    device_func <- switch(format,
      pdf = grDevices::pdf,
      png = function(...) grDevices::png(..., bg = bg),
      svg = function(...) grDevices::svg(...),
      tiff = function(...) grDevices::tiff(..., bg = bg),
      jpeg = function(...) grDevices::jpeg(..., bg = bg),
      eps = grDevices::postscript,
      ps = grDevices::postscript,
      NULL
    )
  }

  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
    if (is.null(plot)) {
      rlang::abort("No plot found. Either provide a plot object or create a ggplot first.")
    }
  }

  ggplot2::ggsave(
    filename = full_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    scale = scale,
    limitsize = limitsize,
    bg = bg,
    device = device_func
  )

  saved_path <- normalizePath(full_path, winslash = "/", mustWork = TRUE)

  if (verbose) {
    message("Saved plot to: ", saved_path)
  }

  invisible(saved_path)
}
