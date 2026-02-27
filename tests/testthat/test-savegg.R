test_that("savegg validates filename parameter", {
  skip_if_not_installed("ggplot2")
  expect_error(savegg(filename = 123), "`filename` must be a single character string")
  expect_error(savegg(filename = c("a.png", "b.png")), "`filename` must be a single character string")
})

test_that("savegg validates dimension parameters", {
  skip_if_not_installed("ggplot2")
  expect_error(savegg(filename = "test.png", width = -1), "`width` must be a positive number")
  expect_error(savegg(filename = "test.png", height = -1), "`height` must be a positive number")
  expect_error(savegg(filename = "test.png", dpi = -1), "`dpi` must be a positive number")
})

test_that("savegg validates format parameter", {
  skip_if_not_installed("ggplot2")
  expect_error(savegg(filename = "test.png", format = "invalid"), "`format` must be one of")
})

test_that("savegg validates plot parameter", {
  skip_if_not_installed("ggplot2")
  expect_error(savegg(plot = 123, filename = "test.png"), "must be a ggplot object")
})

test_that("savegg infers format from filename extension", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  temp_file <- tempfile(fileext = ".png")
  
  result <- savegg(p, temp_file, width = 5, height = 4, verbose = FALSE)
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("savegg creates directory when create_dir is TRUE", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  temp_dir <- file.path(tempdir(), "test_savegg_dir")
  temp_file <- file.path(temp_dir, "plot.png")
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  result <- savegg(p, temp_file, width = 5, height = 4, create_dir = TRUE, verbose = FALSE)
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(temp_file))
})

test_that("savegg saves with specified format", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  temp_file <- tempfile(fileext = ".png")
  
  result <- savegg(p, temp_file, width = 5, height = 4, format = "png", verbose = FALSE)
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("savegg returns file path invisibly", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  temp_file <- tempfile(fileext = ".png")
  
  result <- savegg(p, temp_file, width = 5, height = 4, verbose = FALSE)
  expect_equal(result, normalizePath(temp_file, winslash = "/", mustWork = TRUE))
  unlink(temp_file)
})
