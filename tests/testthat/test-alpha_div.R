test_that("alpha_div validates input", {
  skip_if_not_installed("vegan")
  expect_error(alpha_div("invalid"), "must be a numeric vector or matrix")
  expect_error(alpha_div(c(-1, 2, 3)), "contains negative values")
  expect_error(alpha_div(c(1, 2, 3), indices = "invalid"), "should be one of")
})

test_that("alpha_div calculates shannon index", {
  skip_if_not_installed("vegan")
  x <- c(10, 5, 3, 2, 1)
  result <- alpha_div(x, indices = "shannon")
  expect_true("shannon" %in% names(result))
  expect_true(is.numeric(result$shannon))
  expect_true(result$shannon > 0)
})

test_that("alpha_div calculates simpson index", {
  skip_if_not_installed("vegan")
  x <- c(10, 5, 3, 2, 1)
  result <- alpha_div(x, indices = "simpson")
  expect_true("simpson" %in% names(result))
  expect_true(result$simpson >= 0 && result$simpson <= 1)
})

test_that("alpha_div calculates richness", {
  skip_if_not_installed("vegan")
  x <- c(10, 5, 0, 0, 1)
  result <- alpha_div(x, indices = "richness")
  expect_equal(result$richness, 3)
})

test_that("alpha_div works with matrix input", {
  skip_if_not_installed("vegan")
  mat <- matrix(c(10, 5, 3, 2, 1,
                  8, 6, 4, 0, 0), nrow = 2, byrow = TRUE)
  result <- alpha_div(mat, indices = c("shannon", "richness"))
  expect_equal(nrow(result), 2)
  expect_true(all(c("shannon", "richness") %in% names(result)))
})

test_that("alpha_div calculates all indices", {
  skip_if_not_installed("vegan")
  x <- c(10, 5, 3, 2, 1, 0, 0)
  result <- alpha_div(x, indices = "all")
  expected_cols <- c("shannon", "simpson", "invsimpson", "richness", "chao1", "ace", "fisher")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("alpha_div handles taxon parameter", {
  skip_if_not_installed("vegan")
  x <- c(10, 5, 3, 2, 1)
  result <- alpha_div(x, indices = "shannon", taxon = "Bacteria")
  expect_true("taxon" %in% names(result))
  expect_equal(result$taxon, "Bacteria")
})

test_that("alpha_div handles zero abundances", {
  skip_if_not_installed("vegan")
  x <- c(0, 0, 0)
  result <- alpha_div(x, indices = "richness")
  expect_equal(result$richness, 0)
})

test_that("alpha_div handles single species", {
  skip_if_not_installed("vegan")
  x <- c(0, 10, 0, 0)
  result <- alpha_div(x, indices = c("richness", "shannon"))
  expect_equal(result$richness, 1)
})
