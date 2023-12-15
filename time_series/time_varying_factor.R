# パラメタの設定
N <- 500 # サンプルサイズ

# dlmMLE の推定値の正解値
observationErrorSD <- 20 # 観測誤差の標準偏差
processErrorSD <- 10 # 過程誤差の標準偏差
coefErrorSD <- 0.5 # 係数の過程誤差の標準偏差

# 説明変数をシミュレーション用に作成
set.seed(1)
explanatory <- rnorm(n = N, mean = 10, sd = 10)

# slope（傾き）が毎日過程誤差が加わって、変化していくようにする
set.seed(12)
slope <- cumsum(rnorm(n = N, mean = 0, sd = coefErrorSD)) + 10 # 過程誤差が毎日積み上がる.
plot(slope, main = "時間によるslopeの変化", xlab = "day")

# 同様にしてincetercept（切片）も作成
set.seed(3)
intercept <- cumsum(rnorm(n = N, mean = 0, sd = processErrorSD))
plot(intercept, main = "時間によるinterceptの変化", xlab = "day")

# responseのシミュレーション
set.seed(4)
response <- intercept + explanatory * slope + rnorm(n = N, mean = 0, sd = observationErrorSD)
plot(response, main = "response のシミュレーション結果", xlab = "day")


# 時変係数DLM. closure
buildDlmReg <- function(theta) {
  dlmModReg(
    X = explanatory,
    # 観測誤差の分散. 負を取らないから、exp
    dV = exp(theta[1]),
    # 過程誤差の分散 過程誤差は２つあるため、パラメタも２つ. 同様にexp を使う
    dW = c(exp(theta[2]), exp(theta[3]))
  )
}

# モデル推定
# parm: vector of initial values - for the optimization routine - of the unknown parameters.
# $par が推定した値（観測誤差、過程誤差1、過程誤差2）. exp をとると分散, そのsqrt をとると標準偏差
fitDlmReg <- dlmMLE(response,
  parm = c(2, 1, 1),
  buildDlmReg,
  method = "SANN"
)

# 推定値でモデルを組み直す
modDlmReg <- buildDlmReg(fitDlmReg$par)
# あとはフィルタリング・スムージングして完了
filterDlmReg <- dlmFilter(response, modDlmReg)
smoothDlmReg <- dlmSmooth(filterDlmReg)

plot(response,
  col = 1,
  lwd = 1
)
lines(dropFirst(filterDlmReg$m)[, 1] + dropFirst(filterDlmReg$m)[, 2] * explanatory,
  col = alpha(2, 0.5),
  lwd = 2
)
lines(dropFirst(smoothDlmReg$s)[, 1] + dropFirst(smoothDlmReg$s)[, 2] * explanatory,
  col = alpha(4, 0.5),
  lwd = 2
)

legend(
  "bottomright",
  pch = c(1, NA, NA),
  col = c(1, 2, 4),
  lwd = c(1, 2, 2),
  legend = c("data", "Filter", "Smooth")
)

# 時変係数を推測できてるか？
plot(dropFirst(filterDlmReg$m)[, 2], type = "l", ylim = c(0, 15), col = alpha(2, 0.5), xlab = "day", ylab = "slope")
lines(dropFirst(smoothDlmReg$s)[, 2], type = "l", ylim = c(0, 15), col = alpha(4, 0.5))

# 正しいslope
lines(slope, col = alpha(1, 0.5))
legend(
  "topright",
  col = c(2, 4, 1),
  lty = 1,
  legend = c("フィルタリングのslope", "スムージングのslope", "正しいslope")
)
