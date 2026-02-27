test_that("create_dir creates directory", {
  temp_dir <- file.path(tempdir(), "test_create_dir")
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  result <- create_dir(temp_dir)
  expect_true(dir.exists(temp_dir))
  expect_equal(result, normalizePath(temp_dir, winslash = "/"))
})

test_that("create_dir returns relative path when absolute = FALSE", {
  temp_dir <- file.path(tempdir(), "test_create_dir_rel")
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  result <- create_dir(temp_dir, absolute = FALSE)
  expect_true(dir.exists(temp_dir))
  expect_equal(result, temp_dir)
})

test_that("create_dir handles existing directory", {
  temp_dir <- file.path(tempdir(), "test_create_dir_exists")
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  dir.create(temp_dir)
  result <- create_dir(temp_dir)
  expect_true(dir.exists(temp_dir))
})

test_that("create_dir creates nested directories with recursive = TRUE", {
  temp_dir <- file.path(tempdir(), "test_create_dir_nested", "subdir", "deep")
  on.exit(unlink(file.path(tempdir(), "test_create_dir_nested"), recursive = TRUE))
  
  result <- create_dir(temp_dir, recursive = TRUE)
  expect_true(dir.exists(temp_dir))
})

test_that("create_dir validates input", {
  expect_error(create_dir(123), "`path` must be a single character string")
  expect_error(create_dir(c("a", "b")), "`path` must be a single character string")
  expect_error(create_dir(""), "`path` cannot be empty")
})
