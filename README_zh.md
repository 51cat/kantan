# kantan

<!-- badges: start -->
<!-- badges: end -->

一个简单的 R 包，提供数据可视化和生态学分析的实用函数。

## 安装

从 GitHub 安装开发版本：

```r
# install.packages("devtools")
devtools::install_github("51cat/kantan")
```

## 函数

### 配色方案

```r
library(kantan)

# 连续型配色（用于热图）
seq_colors(10)
seq_colors(20, palette = "plasma")
seq_colors(10, palette = "blues", reverse = TRUE)

# 离散型配色（用于分类数据）
x <- c("A", "B", "C", "A", "B")
disc_colors(x)
disc_colors(x, palette = "dark2")
disc_colors(x, named = TRUE)  # 返回带名称的向量
```

### 文件操作

```r
# 创建文件夹并返回路径
create_dir("output")
create_dir("output/results", absolute = FALSE)

# 保存 ggplot 对象
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
savegg(p, "plot.png")
savegg(p, "output/plot.pdf", width = 12, height = 6, dpi = 600)
```

### 生态学分析

```r
# Alpha 多样性指数
library(vegan)
x <- c(10, 5, 3, 2, 1, 0, 0)
alpha_div(x)
alpha_div(x, indices = c("shannon", "chao1", "ace"))
alpha_div(x, indices = "all")
```

### 进度条

```r
# 在循环中显示进度
for (i in 1:100) {
  Sys.sleep(0.02)
  progress_bar(i, 100, prefix = "处理中:")
}
# 处理中: [==================================================] 100.00% | Elapsed: 2s
```

## 可用配色

### 连续型配色 (seq_colors)

`viridis`, `plasma`, `inferno`, `magma`, `cividis`, `heat`, `cool`, `blues`, `greens`, `reds`, `purples`, `oranges`, `greys`

### 离散型配色 (disc_colors)

`set1`, `set2`, `set3`, `paired`, `dark2`, `accent`, `pastel1`, `pastel2`, `category10`, `tableau`, `okabeIto`

## 依赖

- **Imports**: `grDevices`, `rlang`
- **Suggests**: `ggplot2`, `vegan`, `testthat`

## 许可证

MIT License
