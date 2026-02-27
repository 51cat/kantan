# kantan

<!-- badges: start -->
<!-- badges: end -->

A simple R package providing utility functions for data visualization and ecological analysis.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("51cat/kantan")
```

## Functions

### Color Palettes

```r
library(kantan)

# Sequential colors for heatmaps
seq_colors(10)
seq_colors(20, palette = "plasma")
seq_colors(10, palette = "blues", reverse = TRUE)

# Discrete colors for categorical data
x <- c("A", "B", "C", "A", "B")
disc_colors(x)
disc_colors(x, palette = "dark2")
disc_colors(x, named = TRUE)  # Return named vector
```

### File Operations

```r
# Create directory and return path
create_dir("output")
create_dir("output/results", absolute = FALSE)

# Save ggplot objects
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
savegg(p, "plot.png")
savegg(p, "output/plot.pdf", width = 12, height = 6, dpi = 600)
```

### Ecological Analysis

```r
# Alpha diversity indices
library(vegan)
x <- c(10, 5, 3, 2, 1, 0, 0)
alpha_div(x)
alpha_div(x, indices = c("shannon", "chao1", "ace"))
alpha_div(x, indices = "all")
```

## Available Palettes

### Sequential Palettes (seq_colors)

`viridis`, `plasma`, `inferno`, `magma`, `cividis`, `heat`, `cool`, `blues`, `greens`, `reds`, `purples`, `oranges`, `greys`

### Discrete Palettes (disc_colors)

`set1`, `set2`, `set3`, `paired`, `dark2`, `accent`, `pastel1`, `pastel2`, `category10`, `tableau`, `okabeIto`

## Dependencies

- **Imports**: `grDevices`, `rlang`
- **Suggests**: `ggplot2`, `vegan`, `testthat`

## License

MIT License
