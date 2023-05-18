# > Rscript gaussian-process/main.r
library(MASS)
library(kernlab)

# https://rdrr.io/cran/kernlab/man/kernelMatrix.html
# http://www.kana-lab.c.titech.ac.jp/lecture/lec_2018_osaka/note_01-kernelRegression.pdf

# rbfdot <- function(x1, x2, th1 = 1, th2 = 1) {
#   dis <- abs(x1 - x2)
#   ret <- th1 / th2 * exp(-dis**2)
#   return(ret)
# }
X <- seq(from = 1, to = 4, by = 0.2)
gram_mat <- kernelMatrix(rbfdot(sigma = 1), X) # グラム行列
ret <- mvrnorm(n = 1, X, gram_mat) # 多変量ガウス分布からのサンプリング
plot(X, ret) # 曲線は滑らかになる. 曲線の形は毎回変わる.
