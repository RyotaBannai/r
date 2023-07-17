source("~/Documents/dev/r/r/time_series/kalman_filter/help.R")

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
  result = data.frame(xFiltered = xFiltered,
                      pFiltered = pFiltered)
  
  return(result)
}

localLevelModel = with_docstring(.localLevelModel)
help(localLevelModel)


calcState = function(data, x0, P0, sigmaW, sigmaV) {
  # カルマンフィルタを使って、状態を一気に推定する
  N = length(data)  # サンプルサイズ
  P = numeric(N)# 状態の予測誤差の分散
  P = c(P0, P)# 「状態の予測誤差の分散」の初期値の設定
  x = numeric(N)　# 状態の推定値
  x = c(x0, x)# 「状態」の初期値の設定
  
  # カルマンフィルタの逐次計算を行う
  for (i in 1:N) {
    ret =
      localLevelModel(data[i], x[i], P[i], sigmaW = sigmaW, sigmaV = sigmaV)
    x[i + 1] = ret$xFiltered
    P[i + 1] = ret$pFiltered
  }
  
  # 推定された状態を返す
  return(x[-1])
}

library(ggplot2)
year = 1871:1970

# 色々とパラメタを変えて状態を推定してみる
# パラメタはすでにわかっていること前提で計算
# 観測方程式・状態方程式におけるノイズの分散は「最尤法」と呼ばれる手法を用いて推定される
# 初期値に依存しない方法として「散漫なカルマンフィルタ」という計算方法もある
x1 =
  calcState(
    data = Nile,
    x0 = 0,
    P0 = 1000,
    sigmaW = 1000,
    sigmaV = 10000
  )
# 「状態の予測誤差の分散」の初期値を増やした
x2 =
  calcState(
    data = Nile,
    x0 = 0,
    P0 = 100000,
    sigmaW = 1000,
    sigmaV = 10000
  )
# 状態方程式のノイズの分散をとても小さくした
x3 =
  calcState(
    data = Nile,
    x0 = 1000,
    P0 = 0.1,
    sigmaW = 0.001,
    sigmaV = 1000000
  )
# 観測方程式のノイズの分散をとても小さくした
x4 =
  calcState(
    data = Nile,
    x0 = 1000,
    P0 = 100000,
    sigmaW = 10000,
    sigmaV = 100
  )

dat = data.frame(
  x1 = x1,
  x2 = x2,
  x3 = x3,
  x4 = x4,
  y = Nile,
  year = year
)

# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://r-graph-gallery.com/ggplot2-color.html
ggplot(dat, aes(x = year)) +
  geom_line(aes(y = y), alpha = 0.2, linetype = "twodash") +
  geom_line(aes(y = x1), alpha = 0.5, color = "darkred") +
  geom_line(aes(y = x2), alpha = 0.5, color = "steelblue") +
  geom_line(aes(y = x3), alpha = 0.5, color = "darkorange") +
  # 観測誤差が小さいと、状態が大きく補正されるため観測値と全く同じような値になる.
  geom_line(aes(y = x4), alpha = 0.5, color = 'green') +
  ggtitle("カルマンフィルタの計算結果") +
  xlab("year") + ylab("ナイル川流量")

# plot を使うとき
# for (c in 1:4) {
#   colm = paste('x',c,sep="" )
#   lines(xs[[colm]] ~ year, type = 'l', col=c )
# }


library(dlm)
x5 <-
  calcState(
    data = Nile,
    x0 = 0,
    P0 = 10000000,
    sigmaW = 1000,
    sigmaV = 10000
  )
# dlmのパラメタの設定
modelDlm <-
  dlmModPoly(
    order = 1,
    m0 = 0,
    C0 = 10000000,
    dW = 1000,
    dV = 10000
  )
# カルマンフィルタの実行
Filter <- dlmFilter(Nile, modelDlm)

dat = data.frame(
  x5 = x5,
  # 初期値が残ったままなので、最初のデータは切り落とす
  Filter =  as.numeric(Filter$m)[-1],
  y = Nile,
  year = year
)

# 結果の比較
sum((Filter$m[-1] - x5) ^ 2)

ggplot(dat, aes(x = year)) +
  geom_line(aes(y = y), alpha = 0.2, linetype = "twodash") +
  geom_line(aes(y = Filter), alpha = 0.5, color = "darkred") +
  geom_line(aes(y = x5), alpha = 0.5, color = "steelblue") +
  ggtitle("カルマンフィルタの計算結果") +
  xlab("year") + ylab("ナイル川流量")


# ローカルレベルモデル以外のモデルは、計算が若干複雑になるが考え方はあまり変わらない
# ただし、行列演算が必要になる点だけ注意
# 行列演算ができるライブラリをあらかじめ探しておくといろいろと捗る