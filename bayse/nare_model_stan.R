# ==================================================
# シミュレーションデータの作成
# 収縮期血圧をシミュレーションします
# ==================================================

# ------------------------------
# 薬を投与しなかった時の「状態」
# ------------------------------


N_NotMedicine = 10              # 薬を投与しなかった日数
N_Medicine = 100                # 薬を投与した日数
N = N_NotMedicine + N_Medicine  # 合計日数
muZero = 160                    # 初期の血圧の「状態」

# 血圧の「状態」のシミュレーション
mu = numeric() # 単なるランダムウォーク

set.seed(12)
mu[1] = rnorm(1, mean = muZero, sd = 3)
for (i in 2:N) {
  mu[i] = rnorm(1, mean = mu[i - 1], sd = 3)
}
plot(mu, type = "b")


# --------------------------------------------------
# 時間によって変化する（慣れる）薬の効果のシミュレーション
# --------------------------------------------------

# 薬を使っても、徐々に血圧は下がらなくなっていく
coefMedicineTrendZero = 0.005
coefMedicine = numeric(N_Medicine)　# 時間的に変化する薬の効果
coefMedicineTrend = numeric(N_Medicine) # 単なるランダムウォーク

# 薬の効果をトレンドモデルで表す
set.seed(1)
coefMedicineTrend[1] = rnorm(1, mean = coefMedicineTrendZero, sd = 0.03)

# トレンドのシミュレーション
for (i in 2:N_Medicine) {
  coefMedicineTrend[i] = rnorm(1, mean = coefMedicineTrend[i - 1], sd = 0.03)
}
plot(coefMedicineTrend, type = "b")

# 薬の効果のシミュレーション

# 薬の効果の初期値.
# 初期は、薬の投与によって、血圧が-25下がる. 徐々にこの効果が薄れていく.
coefMedicineZero = -25
coefMedicine[1] = rnorm(1, mean = coefMedicineTrend[1] + coefMedicineZero, sd = 0.5)
for (i in 2:N_Medicine) {
  coefMedicine[i] = rnorm(1, mean = coefMedicineTrend[i] + coefMedicine[i - 1], sd = 0.5)
}

plot(coefMedicine, type = "b")


# 実際の薬の効果は、さらにノイズが加わるとする
# coefMedicine は、（状態空間モデルで考えると）本当の状態として捉えて、
# coefMedicineRealは、状態に対して観測誤差が加わったもの、と捉えることができる.
coefMedicineReal = numeric(100)
set.seed(1)
for (i in 1:100) {
  # 薬の効き目にランダム性を加える
  coefMedicineReal[i] = rnorm(1, mean = coefMedicine[i], sd = 2)
}

plot(coefMedicineReal, type = "b", ylab = "薬の効果（マイナスだと血圧が下がる）")
lines(coefMedicine, col = 2)
legend(
  "topleft",
  legend = c("薬の効果", "薬の効果のトレンド"),
  col = c(1, 2),
  lwd = 1
)

# -------------------------------
# 血圧の観測値のシミュレーション
# -------------------------------

# 最初の10日は薬なし
# 70日後に薬を倍にした
# 100日後に薬を3倍にした
medicine = c(rep(0, N_NotMedicine), rep(1, 60), rep(2, 30), rep(3, 10))

bloodPressure = numeric(N)
bloodPressureMean = numeric(N) #

set.seed(1)

# 最初の10日は薬なし
for (i in 1:N_NotMedicine) {
  bloodPressureMean[i] = mu[i]
  bloodPressure[i] = rnorm(1, mean = bloodPressureMean[i], sd = 10)
}

# 薬を投与した後の血圧のシミュレーション
for (i in (N_NotMedicine + 1):N) {
  bloodPressureMean[i] = mu[i] + coefMedicineReal[i - 10] * medicine[i]
  # 薬の投与後の血圧変動にもランダム性を加える.
  bloodPressure[i] = rnorm(1, mean = bloodPressureMean[i], sd = 10)
}

# 最初は薬を入れると、血圧がガクッと下がるが、徐々に血圧が上がっていく
# 薬を増やすと血圧は下がるが、徐々に効果がなくなっていく
plot(bloodPressure, type = "b")
# 薬投与量を変えた日付を縦線を入れて可視化する
abline(v = 10)
abline(v = 70)
abline(v = 100)
# 薬がなかった時の血圧の「状態」
lines(mu, col = 2, lwd = 2)
lines(bloodPressureMean,
      col = 3,
      lty = 2,
      lwd = 2)
legend(
  "bottomright",
  legend = c("観測値", "薬がなかった時の「状態」", "薬が入った時の「状態」"),
  lwd = c(1, 2, 2),
  col = c(1, 2, 3),
  lty = c(1, 1, 2)
)

# これらの状態をstan によって推定する
require(rstan)

# 計算の並列化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanData = list(
  N_Medicine = N_Medicine,
  N = N,
  bloodPressure = bloodPressure,
  medicine = medicine
)

set.seed(1)
time_variant_coef_model_1 = stan(
  file = './Documents/dev/r/r/bayse/model_blood_pressure.stan',
  data = stanData,
  iter = 45000,
  warmup = 35000,
  thin = 10,
  chains = 3,
  control = list(adapt_delta = 0.9,
                 max_treedepth = 13)
)

# 係数の時間的変化

# index の抜き出しはmatrix のままでできる 
library(stringr) # for str_detect
summaryData = summary(time_variant_coef_model_1)$summary
mask = str_detect(rownames(summaryData), 'coefMedicine') 

# dplyr を使った場合の行のフィルタリング
# library('dplyr')
# df = as.data.frame(summaryData)
# df = filter(df, str_detect(rownames(df), "coefMedicineReal"))


# mean column:1, lower2.5%:4, upper97.5%:8
coefEAP = summaryData[216:315,1] 
coefLower95 = summaryData[216:315,4] # 
coefUpper95 = summaryData[216:315,8]

# 係数の変化の図示
library("ggplot2")

# 描画したいデータをまとめる
d1 = data.frame(day = 1:100, coef = coefMedicine[1:100])
d2 = data.frame(
  day = 1:100,
  EAP = coefEAP[1:100],
  lower = coefLower95[1:100],
  upper = coefUpper95[1:100]
)
head(d1)
head(d2)

# グラフの外枠の作成
coefGraph = ggplot(data = d1, aes(x = day, y = coef))

# データ点を追加
coefGraph = coefGraph + geom_point()

# EAP推定量と95%信用区間を追加
coefGraph = coefGraph + geom_smooth(
  aes(ymin = lower, ymax = upper, y = EAP),
  lwd = 1.2,
  color = 1,
  data = d2,
  stat = "identity"
)

# 描画
plot(coefGraph)

