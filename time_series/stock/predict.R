library(httr)
library(jsonlite)
library(dplyr)
library(quantmod)
library(forecast)

# -----------------------------------------------------------------------
# stock データ前処理
# -----------------------------------------------------------------------

# get regreshToken
# BODY="{\"mailaddress\":\"{your email}\", \"password\":\"{your password}\"}"
# curl -X POST -H "Content-Type: application/json" -d "$BODY" https://api.jquants.com/v1/token/auth_user

# get idToken
# curl -XPOST "https://api.jquants.com/v1/token/auth_refresh?refreshtoken=$refreshToken"

# do request
# curl https://api.jquants.com/v1/listed/info -H "Authorization: Bearer $idToken"

# recommended to add idToken to .env file and reuse it.

# 環境変数読み込み
readRenviron('time_series/.env')
idToken = Sys.getenv('TOKEN')

# You can get information on listed companies and sector information
list_url = 'https://api.jquants.com/v1/listed/info'
# daily stock data

make_stock_url = function(code, from, to) {
  url = paste(
    'https://api.jquants.com/v1/prices/daily_quotes?code=',
    code,
    '&from=',
    from,
    '&to=',
    to,
    sep = ""
  )
  return(url)
}

getStockData = function(url, key) {
  r = GET(url = url,
          add_headers(`Authorization` = paste("Bearer", idToken)),
          verbose())
  # jsonResponseText <- content(r, as = "text",encoding="UTF-8")
  jsonResponseParsed <-
    content(r, as = "parsed", encoding = "UTF-8")
  df = bind_rows(jsonResponseParsed[key]) # make df from  list of lists of key value pairs
  return(df)
}

daily_stock_url = make_stock_url(code = '8411', from = '20210601', to =
                                   '')
df = getStockData(daily_stock_url, "daily_quotes")
# c("Date", "Open", "High", "Low", "Close", "Volume", "Volume_money")
names(df)
df = select(.data = r, Date, Open, High, Low, Close, Volume, TurnoverValue) # 欲しいカラムだけ抜き出す
colnames(df) <-
  c("Date", "Open", "High", "Low", "Close", "Volume", "Volume_money")　# 列名称を変更(ほとんど同じ)

# xts形式に変換
mizuho_xts <- as.xts(read.zoo(df))
# ローソク足のグラフを描く
chartSeries(mizuho_xts,
            type = "candlesticks",
            TA="addSMA(n=25, col='white'); addSMA(n=75, col='green'); addVo()") # 移動平均の線を入れる.

# 対数差分系列にする. (和分系列を元に戻す、対数をとることで、データを正規分布に合わせる）
log_diff = diff(log(mizuho_xts$Close))[-1]  #0 index 目は存在しなく、差分はNA になるから先頭を削除
# 訓練データとテストデータに分ける
train = log_diff["::2022-12-31"]
test = log_diff["2023-01-01::"]


# -----------------------------------------------------------------------
# ARIMAモデルによる予測
# -----------------------------------------------------------------------

# ARIMA モデルの推定
model_arima =　auto.arima(
  train,
  ic = 'aic',
  # ARIMA(p,d,q)のpやd、qといった次数を決めることをモデル選択と呼ぶ
  stepwise = F,
  approximation = F,
  # ARモデルの最大次数
  max.p = 10,
  # MAモデルの最大次数
  max.q = 10,
  # Maximum value of p+q+P+Q if model selection is not stepwise. https://pkg.robjhyndman.com/forecast/reference/auto.arima.html
  max.order = 20,
  parallel = T,
  num.cores = 4,
)

# 予測
f_arima <- forecast(model_arima, h = length(test))

# ナイーブな予測も併せて作成
f_rw <- rwf(train, h = length(test)) # random walk
f_mean <- meanf(train, h = length(test))

# 結果をみるとARIMAモデルは予測の役に立たないことがわかる.
accuracy(f_arima, test)
accuracy(f_rw, test)
accuracy(f_mean, test)


# 2つの予測の比較
#ar(mfrow=c(3,1), mar=c(2.5,4,2.5,4))

plot(f_arima)
#plot(f_rw)
#plot(f_mean)

# par(mfrow=c(1,1))



## 1期先予測なら精度は向上するか

# データを入れると、1期先の予測をしてくれる関数
calcForecast <- function(data) {
  model <- Arima(data, order = c(2, 0, 2), method = "ML")
  return(forecast(model, h = 1)$mean)
}
# 一期先予測
# "non-stationary seasonal AR part from CSS" error in R. https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r
f_arima_2 = rollapply(log_diff, length(train), calcForecast)

# 1期先の予測値なので、実際の日付とずれている。
# lagを使って実際の日付に合わせる（330 のデータは331 の予測値だから、330->331 へlagする.）
f_arima_2 = lag(f_arima_2)
# NA を消す
na.omit(f_arima_2) # f_arima_2[!is.na(f_arima_2)]

# 平均値予測
f_mean_2 <- rollapply(log_diff, length(train), mean)
# 1期先の予測値なので、実際の日付とずれている。
# lagを使って実際の日付に合わせる
f_mean_2 <- lag(f_mean_2)
# NAを消す
f_mean_2 <- f_mean_2[!is.na(f_mean_2)]


# 1期前予測（単純に１日前を使う）
f_rw_2 <- lag(test)
f_rw_2 <- f_rw_2[!is.na(f_rw_2)]

# 図示
plot(log_diff["2022-09::"], main = "みずほHG終値の対数差分系列")
lines(f_arima_2, col = 2, lwd = 2)
lines(f_mean_2, col = 4, lwd = 2)
lines(f_rw_2, col = 5, lwd = 2)
