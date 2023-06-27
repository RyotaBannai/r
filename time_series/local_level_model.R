# ローカルレベルモデル
library(dlm)

# order=1ならローカルレベルモデル（ランダムウォーク　プラス　ノイズモデル）
# order=2ならローカル線形トレンドモデル
# dV:観測誤差の大きさ
# dW:状態の変動の大きさ
build.1 <- function(theta) {
  dlmModPoly(
    order = 1,
    dV = exp(theta[1]),
    dW = exp(theta[2]),
    m0 = exp(theta[3])
  )
}

# MLEでパラメタ推定（最尤法が適用）
# 多段階最適化を適用
fit.1 <- dlmMLE(
  Nile,
  parm = dlmMLE(Nile, parm = c(1, 1, 10), build.1, method = "Nelder-Mead")$par,
  build.1,
  method = "BFGS"
)

DLM <- build.1(fit.1$par)
NileFilt <- dlmFilter(Nile,DLM)　# カルマンフィルタ
NileSmooth <- dlmSmooth(NileFilt) # スムージング


# 引っ張った線はナイル川の流量の見えない「状態」を表す
#１９２０年の流量の見えない「状態」を１９２０年までの観測結果から推定したのが赤い線（フィルタリングの結果）
# 手持ちの100年間のデータすべてをフルに使って状態を推定したのが青い線（スムージング）の結果
plot(Nile, type="o", col=8, ylab="", main="Nile Filtering") # ナイル川の流量データ
lines(dropFirst(NileFilt$m), col=2, lwd=2)
lines(dropFirst(NileSmooth$s), col=4, lwd=2)
