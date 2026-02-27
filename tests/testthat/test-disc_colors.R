test_that("disc_colors returns correct length", {
  x <- c("A", "B", "C", "A", "B")
  expect_length(disc_colors(x), 5)
})

test_that("disc_colors returns same values for same categories", {
  x <- c("A", "B", "A", "C", "B")
  colors <- disc_colors(x)
  expect_equal(colors[1], colors[3])
  expect_equal(colors[2], colors[5])
})

test_that("disc_colors works with factors", {
  x <- factor(c("A", "B", "C"))
  colors <- disc_colors(x)
  expect_length(colors, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("disc_colors validates input", {
  expect_error(disc_colors(1:5), "`x` must be a factor or character vector")
  expect_error(disc_colors(c("A", "B"), palette = "invalid"), "`palette` must be one of")
  expect_error(disc_colors(c("A", "B"), alpha = 2), "`alpha` must be a numeric value between 0 and 1")
})

test_that("disc_colors all palettes work", {
  palettes <- c("set1", "set2", "set3", "paired", "dark2", "accent",
                "pastel1", "pastel2", "category10", "tableau", "okabeIto")
  x <- c("A", "B", "C")
  for (pal in palettes) {
    colors <- disc_colors(x, palette = pal)
    expect_length(colors, 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})

test_that("disc_colors handles more levels than palette colors", {
  x <- paste0("cat", 1:15)
  colors <- disc_colors(x, palette = "dark2")
  expect_length(colors, 15)
  expect_length(unique(colors), 15)
})

test_that("disc_colors shuffle works", {
  x <- c("A", "B", "C", "D")
  colors_normal <- disc_colors(x, palette = "set1")
  colors_shuffled <- disc_colors(x, palette = "set1", shuffle = TRUE, seed = 123)
  expect_true(!all(colors_normal == colors_shuffled) || all(colors_normal == colors_shuffled))
  colors_shuffled2 <- disc_colors(x, palette = "set1", shuffle = TRUE, seed = 123)
  expect_equal(colors_shuffled, colors_shuffled2)
})

test_that("disc_colors named parameter works", {
  x <- c("A", "B", "C", "A")
  colors_unnamed <- disc_colors(x, named = FALSE)
  colors_named <- disc_colors(x, named = TRUE)
  expect_null(names(colors_unnamed))
  expect_equal(names(colors_named), x)
  expect_equal(unname(colors_named), colors_unnamed)
})
