test_that("seq_colors returns correct number of colors", {
  expect_length(seq_colors(10), 10)
  expect_length(seq_colors(1), 1)
  expect_length(seq_colors(100), 100)
})

test_that("seq_colors returns valid hex colors", {
  colors <- seq_colors(10)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", colors)))
})

test_that("seq_colors validates input parameters", {
  expect_error(seq_colors(-1), "`n` must be a single positive integer")
  expect_error(seq_colors(1.5), NA)
  expect_error(seq_colors(10, palette = "invalid"), "`palette` must be one of")
  expect_error(seq_colors(10, alpha = 2), "`alpha` must be a numeric value between 0 and 1")
})

test_that("seq_colors reverse parameter works", {
  colors_normal <- seq_colors(10, palette = "viridis")
  colors_reversed <- seq_colors(10, palette = "viridis", reverse = TRUE)
  expect_equal(rev(colors_normal), colors_reversed)
})

test_that("seq_colors alpha parameter works", {
  colors_no_alpha <- seq_colors(10, alpha = 1)
  colors_with_alpha <- seq_colors(10, alpha = 0.5)
  expect_true(nchar(colors_with_alpha[1]) >= nchar(colors_no_alpha[1]))
})

test_that("seq_colors all palettes work", {
  palettes <- c("viridis", "plasma", "inferno", "magma", "cividis",
                "heat", "cool", "blues", "greens", "reds", "purples", "oranges", "greys")
  for (pal in palettes) {
    colors <- seq_colors(10, palette = pal)
    expect_length(colors, 10)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", colors)))
  }
})
