# ランダムウォーク、ドランカーズウォーク、酔歩
# ホワイトノイズの累積和を取ると簡単に作れる
# 和を取っているからランダムウォークは和分過程
# 同じ数値が使われる＝似てる＝自己相関がある
#
#
set.seed(1)
white.noise <- rnorm(n = 400) # 正規分布
random.walk <- cumsum(white.noise)
par(family = "HiraKakuProN-W3") # plot するグラフの日本語化
plot(random.walk, type = "l", main = "和分過程：ランダムウォーク")
acf(random.walk) # 自己相関が無いかどうか確認


model.RW <- auto.arima(
  random.walk,
  ic = "aic",
  trace = T,
  stepwise = F,
  approximation = F
)


plot(forecast(model.RW, h = 100), flwd = 5)
abline(h = mean(random.walk)) # random walk の平均
abline(h = random.walk[400], col = 2, lty = 2) # random walkの最後に得られたデータの大きさ
