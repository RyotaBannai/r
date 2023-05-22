# > Rscript gaussian-process/main.r
library(MASS)
library(kernlab)
library(purrr)

# https://rdrr.io/cran/kernlab/man/kernelMatrix.html
# http://www.kana-lab.c.titech.ac.jp/lecture/lec_2018_osaka/note_01-kernelRegression.pdf

# rbfdot <- function(x1, x2, th1 = 1, th2 = 1) {
#   dis <- abs(x1 - x2)
#   ret <- th1 / th2 * exp(-dis**2)
#   return(ret)
# }
X <- seq(from = 0, to = 4, by = 0.2)
gram_mat <- kernelMatrix(rbfdot(sigma = 1), X) # グラム行列
# 平均0 のガウス過程だから、観測データ（本書3.17ではy）のMVRの平均mu=0 にセット
ret <- mvrnorm(n = 1, rep(0, length(X)), gram_mat) # 多変量ガウス分布からのサンプリング
# 曲線は滑らかになる. 曲線の形は毎回変わるが、近いX に対しては共分散行列も大きいためy の値も近くなる
plot(X, ret)
