# ホワイトノイズ
# 自己相関の無い完全な雑音のようなデータ
#
set.seed(1)
white.noise <- rnorm(n = 400) # 正規分布
plot(white.noise, type = "l")
acf(white.noise)　# 自己相関が無いかどうか確認

library(forecast)
model.noise <- auto.arima(
  # データの指定
  white.noise,
  # 情報量基準の指定
  ic = "aic",
  # 計算の途中経過を表示させる
  trace = T,
  # 最適な次数を探す際に次数を総当たりで調べる
  stepwise = F,
  # 実際にモデルを計算するときに、近似法を使わない
  approximation = F,
  # parallel = T
)
plot(forecast(model.noise, h=100), flwd=5)
abline(h=mean(white.noise))
