test_that("progress_bar validates input", {
  expect_error(progress_bar("a", 10), "`current` and `total` must be numeric")
  expect_error(progress_bar(5, -1), "`total` must be a positive number")
  expect_error(progress_bar(-1, 10), "`current` must be non-negative")
})

test_that("progress_bar handles basic progress", {
  expect_output(progress_bar(1, 10, prefix = "Test:"), "Test:")
})

test_that("progress_bar handles completion", {
  expect_output(progress_bar(10, 10, prefix = "Test:"), "\n")
})

test_that("progress_bar caps current at total", {
  expect_output(progress_bar(100, 10, prefix = "Test:"), "\n")
})

test_that("progress_bar handles zero current", {
  expect_output(progress_bar(0, 10, prefix = "Test:"), "0.00%")
})

test_that("progress_bar shows time when enabled", {
  expect_output(progress_bar(5, 10, show_time = TRUE), "ETA")
})

test_that("progress_bar hides time when disabled", {
  output <- capture.output(progress_bar(5, 10, show_time = FALSE))
  expect_false(any(grepl("ETA", output)))
})
