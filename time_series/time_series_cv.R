library(forecast)
library(ggfortify)
library(ggplot2)
library(gridExtra)

# 対象データ
# Monthly Airline Passenger Numbers 1949-1960
# 対数変換したものを使う
log_air_pass = log(AirPassengers)
class(log_air_pass)
ggtsdisplay(log_air_pass)
print(log_air_pass, digits = 4)


# 訓練データとテストデータに分割
# 最初の10年を訓練、最後の2年をテストデータとする
train <- window(log_air_pass, end = c(1958, 12))
test <- window(log_air_pass, start = c(1959, 1))

# モデルを作る
mod_sarima <- auto.arima(
  train,
  ic = "aic",
  stepwise = F,
  parallel = TRUE,
  num.cores = 4
)

# 残差の評価
checkresiduals(mod_sarima)

# 予測期間
h <- length(test)
# 予測
f_sarima <- forecast(mod_sarima, h = h)
# 予測結果の図示
autoplot(f_sarima, main = "SARIMAによる予測")

# 予測の評価. train, test それぞれのデータに対する予測評価がわかる.
accuracy(f_sarima, test)
# ナイーブな予測と比較して、整然データを作成. RMSEの誤差を棒グラフなどのにすると視覚的に理解できる.

