test_that("path_exists validates input", {
  expect_error(path_exists(123), "`path` must be a single character string")
  expect_error(path_exists(c("a", "b")), "`path` must be a single character string")
  expect_error(path_exists("test", type = "invalid"), "should be one of")
})

test_that("path_exists checks directory existence", {
  temp_dir <- tempdir()
  expect_true(path_exists(temp_dir))
  expect_true(path_exists(temp_dir, type = "dir"))
  expect_false(path_exists(temp_dir, type = "file"))
})

test_that("path_exists checks file existence", {
  temp_file <- tempfile()
  writeLines("test", temp_file)
  on.exit(unlink(temp_file))
  
  expect_true(path_exists(temp_file))
  expect_true(path_exists(temp_file, type = "file"))
  expect_false(path_exists(temp_file, type = "dir"))
})

test_that("path_exists returns FALSE for nonexistent path", {
  expect_false(path_exists("nonexistent_path_12345"))
})

test_that("path_exists throws error when error = TRUE", {
  expect_error(path_exists("nonexistent_path_12345", error = TRUE), "Path does not exist")
  expect_error(path_exists("nonexistent_path_12345", type = "file", error = TRUE), "File does not exist")
  expect_error(path_exists("nonexistent_path_12345", type = "dir", error = TRUE), "Directory does not exist")
})
