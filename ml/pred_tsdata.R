# ts はテーブルデータっぽいけど、普通に１元配列（/ ベクトル）のようにdiff が取れる.
# log を取らない場合（差分だけの場合）、乗員数の増え方が年々指数的に増えているから振れ幅が大きくなるようなグラフになる.
# log(a) - log(b), つまり a/b のように、対数差分系列は近似的に「変化率」の推移と等しくなるため解釈しなすくなるというメリットがある.
log_diff_passenger <- diff(log(AirPassengers))
# 図示
par(mfrow = c(2, 1))
plot(AirPassengers, main = "原系列")
plot(log_diff_passenger, main = "対数差分系列")
par(mfrow = c(1, 1))

# ML 用にラグをとって、過去のデータを説明変数にする
# embed関数による整形
# （カラム1 を応答変数、それ以外の３つを説明変数とする. 過去３つのデータから予測するモデルにする）
lag_num <- 4
exp_val_sample <- as.data.frame(embed(log_diff_passenger, lag_num))

# 列名の変更
colnames(exp_val_sample)[1] <- "Y"
for (i in 2:lag_num) {
  colnames(exp_val_sample)[i] <- paste("Lag", i - 1, sep = "")
}

library(caret)
library(kernlab)
library(doParallel)
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)

# モデルの構築
set.seed(0)
tuned_svm_sample <- train(
  Y ~ .,
  data = exp_val_sample,
  # サポートベクトル回帰だから、εをチューニングしたいが、caretではできない。。。
  # コストパラメタC, ガウシアンカーネルsigma のみ
  method = "svmRadial",
  # SVM を使う.
  tuneGrid = expand.grid(C = c(1:5), sigma = 2^c(-1:1)),
  # 標準化
  preProcess = c("center", "scale")
)


## 最適な次数を選ぶ
# 予測精度などを格納する入れ物
sim_result <- data.frame(
  order = numeric(),
  error = numeric(),
  C = numeric(),
  sigma = numeric()
)

# 予測モデルの一覧を格納する入れ物
tuned_models <- list()
# 最大ラグ数
lag_max <- 12

# 訓練データの作成
# 最後のデータだけテスト用に残しておく
exp_val <-
  as.data.frame(embed(log_diff_passenger[-length(log_diff_passenger)], lag_max + 1))
colnames(exp_val)[1] <- "Y"
for (i in 2:(lag_max + 1)) {
  colnames(exp_val)[i] <- paste("Lag", i - 1, sep = "")
}

# ループさせて、最も予測精度が高くなるラグ数を調べる（ループの中でベストなパラメタの組も見つける）
for (i in 1:lag_max) {
  # 必要なラグまで、データを切り落とす
  exp_val_tmp <- exp_val[, c(1:(i + 1))]

  # 予測モデルの作成
  tuned_svm <- train(
    Y ~ .,
    data = exp_val_tmp,
    method = "svmRadial",
    tuneGrid = expand.grid(C = c(1:3), sigma = 2^c(-1:1)),
    preProcess = c("center", "scale")
  )

  # 予測モデルを保存する
  tuned_models <- c(tuned_models, list(tuned_svm))

  # 予測精度などを保存する
  sim_result[i, "order"] <- i
  # 最小の予測誤差、すなわち、最も良いパラメタを使った場合の予測誤差だけを管理
  sim_result[i, "error"] <- min(tuned_svm$results$RMSE)
  sim_result[i, "sigma"] <- tuned_svm$bestTune["sigma"]
  sim_result[i, "C"] <- tuned_svm$bestTune["C"]
}

# ラグと予測精度の関係
plot(
  sim_result$error ~ sim_result$order,
  main = "次数と予測誤差の関係",
  xlab = "order",
  ylab = "error"
)


# 最も予測精度が高かったモデルの評価結果
best <- subset(sim_result, sim_result$error == min(sim_result$error)) # subset = dplyr のfilter と等価
# 次数だけを取り出す
best_order <- best$order
## 当てはまりの精度の確認
# 最も予測精度が高かったモデルを取り出す
best_model <- tuned_models[[best_order]]


# 予測値と実測値のプロット
# なお、これは「手持ちのデータへの当てはまり」の精度であることに注意
# 学習に使っていない、1:12,last データをplot から除く#
# .(この範囲はtrainデータへの当てはまり具合を比較できないため)
plot(log_diff_passenger[-c(1:lag_max, length(log_diff_passenger))],
  type =
    "l"
)
lines(predict(best_model),
  col = 2,
  lwd = 2,
  lty = 2
)
legend(
  legend = c("実測値", "予測値"),
  "topright",
  col = c(1, 2),
  lwd = c(1, 2),
  lty = c(1, 2)
)

# train データに使った最後の行の説明変数を取り出す
# nrow はdata.frameの行数. length はカラム数
last_data <- exp_val[nrow(exp_val), 1:best_order]
# 列名を、学習データに合わせる
for (i in 1:best_order) {
  colnames(last_data)[i] <- paste("Lag", i, sep = "")
}

predict(best_model, last_data) # 予測結果
log_diff_passenger[length(log_diff_passenger)] # 正解
