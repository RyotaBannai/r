library(nonlinearTseries)
# nonlinearTsries の関数を使ってロジスティック写像をシミュレートしてみる
# 脈絡もなくグネグネとしており、不規則な変動をしているように見える
logMap = logisticMap(
  r = 3.5,
  n.sample = 100,
  start = 0.4,
  n.transient = 0,
  do.plot = TRUE
)

# ロジスティック曲線
K = 1
b = 3
c = 1
x = seq(-5, 10, 0.1)
y = K / (1 + b * exp(-c * x)) # ロジスティック関数と同じ.
plot(y ~ x, type = 'l', main = 'ロジスティック曲線')

# 一期ずらした値を作る
# 第二引数は一期ずらした系列を何個作るか。
# 例えば、２にすると、２つで１カラム目に一期ずれた位置からの系列が追加される. この時カラム２の最後は無くなる点に注目.
logData = embed(y, 2)
# 差分をとる. 1->2, 2->3 の増加分を計算.カラム１はカラム２よりも同じindex で１期先の値だから、そのまま引く
diffData = logData[, 1] - logData[, 2]
# 放物線を描く！
# これは、ロジスティク曲線のグラフからも自明なように、曲線の中央で大きく0->1へ変化することからも、0.5周辺での変化量が大きくなる.
plot(diffData ~ logData[, 1],
     ylab = 'yの増加値',
     xlab = 'yの値',
     main = "yの増加値とyの値") # ２つ目からplot

# 同様にロジスティック写像もずらしてプロット. 差分ではなく
# ロジスティック写像の時系列データは「前期の値が大きすぎたり小さすぎたりすると、今期は大きな値にならない」
# 逆に「前期の値が中くらいの時に、今期の値は大きくなる」という特徴がある
# この点がロジスティック曲線の性質と対応.
diffLogMap = embed(logMap, 2)
plot(
  diffLogMap[, 1] ~ diffLogMap[, 2],
  ylab = '今期の値',
  xlab = "前期の値",
  main = "今期の値と前期の値の比較"
)


# ------------------------------
# カオス判定 - リアプノフ指数
# ------------------------------

logMapDifferential = function(r, x) {
  # ロジスティック写像の微分係数
  return(-2 * r * x + r)
}
# 微分係数の総和
# cat(body(lyapunovExponent)[[2L]]) でdocstring を表示
lyapunovExponent = function(differentials) {
  r"---(calculates Lyapunov Exponent
  @param differentials: A numeric vector.
  )---"
  return (sum(log(abs(differentials))) / length(differentials))
}

# リアプノフ指数が正の値になれば、初期値が変わることによって、将来の値が変わっていくことがわかる
# 逆に、周期的な変動を示すデータなど、初期値の影響が少ない場合は、リアプノフ指数が負になる
lyapunovExponent(logMapDifferential(4, logMap))

# 係数を少し変えるとカオスでなくなることを確かめる.
logMap2 = logisticMap(
  r = 3.5,
  n.sample = 100,
  start = 0.4,
  n.transient = 0,
  do.plot = TRUE
)
lyapunovExponent(logMapDifferential(3.5, logMap2)) # 負値になることを確認.(初期値依存性が低い)


# ------------------------------
# カオス判定 - サロゲートテスト
# ------------------------------
# リアプノフ指数は、微分係数を計算できることがで前提にある.
# ロジスティック写像は元の関数がわかっているから、微分係数が簡単に求められたが、実データの値しか持っていない時に
# カオス判定したい時は難儀
# リアプノフ指数を使うと、データが線形・非線形なのかの過程判別ができる.
# 線形では、ノイズを加えたときのリアプノフ指数はあまり変化しないが、非線形の場合は、ノイズを加えると大きく異なる.
surrogateTest(
  time.series = logMap,
  significance = 0.05,
  K = 1,
  one.sided = FALSE,
  FUN = timeAsymmetry
)

surrogateTest(
  time.series = rnorm(100),
  significance = 0.05,
  K = 1,
  one.sided = FALSE,
  FUN = timeAsymmetry
)
