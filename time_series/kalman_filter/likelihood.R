source("~/Documents/dev/r/r/time_series/kalman_filter/help.R")

# --------------------------------------------------------------------------------------
# ライブラリを使わないカルマンフィルタの実装
# 尤度を計算するための情報も出力するようにした
# --------------------------------------------------------------------------------------
# cat(body(localLevelModel)[[2L]]) するとdocstring がみれる
.localLevelModel = function(y, xPre, pPre, sigmaW, sigmaV) {
  r"---(カルマンフィルタ
  @param:
    y: 当期の観測値
    xPre: 前期の状態
    pPre: 前期の状態の予測誤差の分散
    sigmaW: 状態方程式のノイズの分散
    sigmaV: 観測方程式のノイズの分散

  @value
    data.frame(
      xFiltered # フィルタリング後の状態
      pFiltered # フィルタリング後の予測誤差の分散
      v         # 観測値の予測誤差
      F         # 観測値の予測誤差の分散
    )
  )---"
  # 状態の予測（フィルター時にはローカルレベルモデルなので予測値は前期の値と同じ）
  xForecast = xPre
  # 状態の予測誤差の分散（分散は時刻が進むにつれ大きくなる）
  pForecast = pPre + sigmaW
  # カルマンゲイン
  kGain = pForecast / (pForecast + sigmaV)
  # カルマンゲインを使って補正された状態（観測値で状態を補正）
  xFiltered = xForecast + kGain * (y - xForecast)
  # 補正された状態の予測誤差の分散（kGain が大きければ、分散が小さくなる）
  pFiltered = (1 - kGain) * pForecast
  
  # 観測値の予測誤差
  v = y - xForecast
  # 観測値の予測誤差の分散
  F = pForecast + sigmaV
  result = data.frame(
    xFiltered = xFiltered,
    pFiltered = pFiltered,
    v = v,
    F = F
  )
  return(result)
}

localLevelModel = with_docstring(.localLevelModel)
help(localLevelModel)


calcState = function(data, x0, P0, sigma) {
  # カルマンフィルタを使って、状態を一気に推定する
  N = length(data)  # サンプルサイズ
  P = numeric(N)# 状態の予測誤差の分散
  P = c(P0, P)# 「状態の予測誤差の分散」の初期値の設定
  x = numeric(N)　# 状態の推定値
  x = c(x0, x)# 「状態」の初期値の設定
  v = numeric(N) # 観測値の予測誤差
  F = numeric(N) # 観測値の予測誤差の分散
  
  
  # 分散は負にならないため、あらかじめEXPをとっておく
  sigmaW <- exp(sigma[1])
  sigmaV <- exp(sigma[2])
  
  # カルマンフィルタの逐次計算を行う
  for (i in 1:N) {
    ret =
      localLevelModel(data[i], x[i], P[i], sigmaW = sigmaW, sigmaV = sigmaV)
    x[i + 1] = ret$xFiltered
    P[i + 1] = ret$pFiltered
    v[i] = ret$v
    F[i] = ret$F
  }
  
  # 推定された状態を返す
  result = data.frame (x = x[-1],
                       v = v,
                       F = F)
  return(result)
}

year = 1871:1970
result =
  calcState(
    data = Nile,
    x0 = 0,
    P0 = 10000000,
    sigma =  c(log(1000), log(10000))
  )

library(zeallot)
# https://cran.r-project.org/web/packages/zeallot/vignettes/unpacking-assignment.html
# unpacking, Destructuring assignment
c(x, v, F) %<-% result # data.frame もそのまま分割できる

# 対数尤度の計算1
# dnorm関数を使った対数尤度の計算
# 以下の結果, maximum likelihood がもとまる.
# この値に近づくようなパラメタ（状態方程式のノイズの分散と、観測方程式のノイズの分散）
# を最適化手法を用いて求める
# dnorm は（観測値の予測誤差、期待値、標準偏差）を引数に、確率密度を計算する（データ一つ一つに対して）
sum(log(dnorm(v, mean = 0, sd = sqrt(F))))
# [1] -646.3254

# 対数尤度の計算２
# 正規分布を仮定しているから、その確率分布を使って計算することもできる
# 対数尤度関数の式
# -(n/2) log2π - nlogσ - (x1-μ)^2 + ... + (xn-μ)^2 / (2σ^2)
# = -(n/2) log2π- (1/2) (logσ + Σv^2/ (σ^2)) <- nlog が 分散の総和になることに注意.
# https://math-note.xyz/statistics/maximum-likelihood-estimation/
N = length(Nile)
- 1 * (N / 2) * log(2 * pi) - 1 / 2 * sum(log(F) + v ^ 2 / F)
# [1] -646.3254
# dlmパッケージで対数尤度を計算しても良い
#

# optim のための対数尤度を返す関数を作っておく
# optim：汎用最適化関数と呼ばれるもので、「計算に使う関数」と「パラメタの初期値」を与えてやると、「計算結果を最小とするパラメタ」を自動で計算してくれる
# -> 対数尤度の負の項を正に置き換えて返す
# https://www.yasuhisay.info/entry/20090516/1242480413
.calcLogLikehoodLocalLevel = function(sigma) {
  r"---(パラメタ（状態方程式、観測方程式の分散）から対数尤度を返す関数
  @param:
    sigma[1]: 状態方程式の分散
    sigma[2]: 観測方程式の分散

  @value
    対数尤度
  )---"
  c(x, v, F) %<-% calcState(
    data = Nile,
    x0 = 0,
    P0 = 10000000,
    sigma = sigma
  )
  # 「対数尤度の最大化」は「この負の項（指標）を最小にする」行為に置き換わる
  # 定数部分は計算を省略した.
  loglikelihood = 1 / 2 * sum(log(F) + v ^ 2 / F)
  return (loglikelihood)
}

calcLogLikehoodLocalLevel = with_docstring(.calcLogLikehoodLocalLevel)
calcLogLikehoodLocalLevel(sigma = c(log(1000), log(10000)))

# optim はデフォルトで入ってる
optSigma = optim(c(1, 1), calcLogLikehoodLocalLevel, method = "L-BFGS-B")

# dlm package を使った同じパラメタの推定
library(dlm)
buildDlm = function(sigma) {
  dlmModPoly(
    order = 1,
    m0 = 0,
    C0 = 10000000,
    dW = exp(sigma[1]),
    dV = exp(sigma[2])
  )
}
fitDlm = dlmMLE(Nile, parm = c(1, 1), buildDlm)
fitDlm # optim を使った場合と同じ値になっていることを確認

# この値をカルマンフィルタのパラメタに使ってあげれば良い.
Filter = result =
  calcState(
    data = Nile,
    x0 = 0,
    P0 = 10000000,
    sigma = optSigma$par
  )


library(ggplot2)
dat = data.frame(Filter =  as.numeric(Filter$x),
                 y = Nile,
                 year = year)

ggplot(dat, aes(x = year)) +
  geom_line(aes(y = y), alpha = 0.2, linetype = "twodash") +
  geom_line(aes(y = Filter), alpha = 0.5, color = "darkred") +
  ggtitle("最尤法で求めた分散を使用したカルマンフィルタの計算結果") +
  xlab("year") + ylab("ナイル川流量")
