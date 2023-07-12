# 売り上げ効果を織り込む
## パラメタの設定
N <- 100       # 期間
user_num <- 10 # ユーザー数
x_0 <- 200     # 状態初期値
s_w <- 20      # 過程誤差の標準偏差
s_v <- 30      # 観測誤差の標準偏差
s_r <- 50      # ランダム効果の標準偏差

b_ad <- 100     # 広告による売り上げ増加効果


x <- matrix(nrow = N)                        # 全ユーザー共通の状態
r <- matrix(ncol = user_num)                 # ユーザーごとのランダム効果
y <- matrix(nrow = N, ncol = user_num)       # ユーザーごとの売り上げ
ad_flag <- matrix(nrow = N, ncol = user_num) # 0なら広告なし。1で広告あり。

# わかりやすくするため、列名を付けておく
colnames(x) <- "state"
colnames(y) <- LETTERS[1:10]
colnames(ad_flag) <- LETTERS[1:10]
colnames(r) <- LETTERS[1:10]

set.seed(2)
r[] <- rnorm(n = user_num, mean = 0, sd = s_r)

# r を見ると、A,D,H,J さんは、平均（０）よりも、売り上げが少なっている
# そこで、この４人に対して重点的に広告を売ったと考える.
# まず、最初の50時点では、全体の3割ほどに、ランダムに広告を入れる
# そして、最後の50時点では、A,D,H,Jさんにだけ広告を配信
ad_flag[1:50,] = sample(
  c(0, 1),
  size = N * user_num / 2,
  replace = T,
  prob = c(0.7, 0.3)
)
# mean(ad_flag[1:50,])
ad_flag[51:100,] <- 0
ad_flag[51:100, c("A", "D", "H", "J")] <- 1

# 乱数の種
set.seed(2) # 何回もセットしないといけない?
# 状態の初期値
x[1, ] <- rnorm(n = 1, mean = x_0, sd = s_w)

# 各時点（i）ごとにループを回して、状態を遷移させる
for (i in 2:N) {
  # 状態の遷移（ランダムウォーク仮定）
  x[i,] <- rnorm(n = 1, mean = x[i - 1], sd = s_w)
}

# 各時点（i）ごと、ユーザー（j）ごとにループを回す
for (i in 1:N) {
  for (j in 1:user_num) {
    # 「状態＋ユーザー固有のランダム効果＋広告効果」を平均値として、
    # 観測ノイズが加わって、観測値が得られる
    y[i, j] <-
      rnorm(n = 1,
            mean = x[i] + r[j] + ad_flag[i, j] * b_ad,
            sd = s_v)
  }
}

# 整然データの形式にする
sim_tidy <- y %>%
  cbind(state = x) %>%
  cbind(time = 1:100) %>%
  data.frame %>%
  gather(key = "name",
         value = "sales",
         factor_key = TRUE,
         -time)
# 図示
ggplot(data = sim_tidy) +
  ggtitle("シミュレーションにより生成された売り上げデータ（モデル２）") +
  geom_line(aes(x = time, y = sales, color = name)) +
  gghighlight(name == "state" | name == "D" | name == "I",
              use_group_by = FALSE)


# データの準備
data_list_2 <- list(
  y = y,
  ad_flag = ad_flag,
  T = N,
  user_num = user_num
)

# 多変量モデルの推定
malti_2 <- stan(
  file = './Documents/dev/r/r/time_series/ssm-user-sales_2.stan',
  data = data_list_2,
  seed = 1,
  iter = 30000,
  warmup = 10000,
  thin = 10
)

mcmc_rhat(rhat(malti_2))


# データの整形
stan_df_2 <- malti_2 %>%
  rstan::extract() %$% x %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(1:nrow(sim_df)) %>%
  data.frame

# 列名の変更
colnames(stan_df_2) <- c("lwr", "fit", "upr", "time")

# 図示
ggplot(data = sim_tidy) +
  ggtitle("推定結果（モデル１）") +
  geom_line(aes(x = time, y = sales, color = name)) +
  gghighlight(name == "state", use_group_by = FALSE) +
  geom_line(data = stan_df_2,
            aes(x = time, y = fit), size = 1.2) +
  geom_ribbon(data = stan_df_2,
              aes(x = time, ymin = lwr, ymax = upr),
              alpha = 0.3)

# 状態と比較
cbind(y_mean = apply(y, 1, mean),
      state = x,
      fit = stan_df_2[, "fit"]) %>%
  ts() %>%
  autoplot(facet = F, main = "状態推定値と平均値の比較")
