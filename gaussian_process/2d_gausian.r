# > Rscript gaussian-process/main.r
library(MASS)
library(kernlab)
library(purrr)
# library(GA)
library(dplyr)
library(plotly)
library(ggplot2)
# library(PrevMap)

# Matrix In R - Adding Rows And Columns To A Matrix In R
# https://www.c-sharpcorner.com/article/matrix-in-r-adding-rows47columns-and-accessing-elements-by-name-in-matrix-i/

base <- seq(from = 0, to = 4, by = 0.2)
X <- c()
for (y in seq(from = 0, to = 4, by = 0.2)) {
  X <- c(X, map(base, function(x) c(x, y))) |> unlist()
}

mat <- matrix(data = X, nrow = length(X) / 2, ncol = 2, byrow = TRUE)
# dim(mat)
# [1] 441   2

ker <- rbfdot(sigma = 1)
# ker <- matern.kernel(rho = 1, kappa = 1.5) # Matern kernel?
gram_mat <- kernelMatrix(ker, mat) # グラム行列
# gram_mat[,1] # slicing 共分散行列の１行目を取得. １次元目が1 になっているのを確認できる
ret <- mvrnorm(n = 1, rep(0, length(X) / 2), gram_mat)
z <- matrix(data = ret, nrow = length(base), ncol = length(base), byrow = TRUE)
df <- data.frame(
  x = base,
  y = base
)
fig <- plot_ly(df, z = z) |> add_surface()
fig

# persp3D(base, base, z, col.palette = rainbow)
# https://luca-scr.github.io/GA/reference/persp3D.html

# rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha, rev = FALSE)
# heat.colors(n, alpha, rev = FALSE)
# terrain.colors(n, alpha, rev = FALSE)
# topo.colors(n, alpha, rev = FALSE)
# cm.colors(n, alpha, rev = FALSE)

# multidimensional plot
# https://stackoverflow.com/questions/22150235/multidimensional-2d-function-plot-in-r
