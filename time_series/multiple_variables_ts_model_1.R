library(tidyverse)
library(magrittr)
library(ggfortify)
library(gghighlight)
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())


## パラメタの設定
N <- 100 # 期間
user_num <- 10 # ユーザー数
x_0 <- 200 # 状態初期値
s_w <- 20 # 過程誤差の標準偏差
s_v <- 30 # 観測誤差の標準偏差
# これは状態空間モデルに入れるのではなく、観測値の平均に加える
s_r <- 50 # ランダム効果の標準偏差

x <- matrix(nrow = N) # 全ユーザー共通の状態（状態）
r <- matrix(ncol = user_num) # ユーザーごとのランダム効果（売り上げ増減効果）
y <- matrix(nrow = N, ncol = user_num) # ユーザーごとの売り上げ（観測値）

# わかりやすくするため、列名を付けておく
colnames(x) <- "state"
colnames(y) <- LETTERS[1:10]
colnames(r) <- LETTERS[1:10]

set.seed(2)
r[] <- rnorm(n = user_num, mean = 0, sd = s_r) # ランダム効果を生成
x[1, ] <- rnorm(n = 1, mean = x_0, sd = s_w) # 状態の初期値
# 状態の遷移（ランダムウォーク仮定）
# 各時点（i）ごとにループを回して、状態を遷移させる
for (i in 2:N) {
  x[i, ] <- rnorm(n = 1, mean = x[i - 1], sd = s_w)
}


# 各時点（i）ごと、ユーザー（j）ごとにループを回す
for (i in 1:N) {
  for (j in 1:user_num) {
    # 「状態＋ユーザー固有のランダム効果」を平均値として、
    # 観測ノイズが加わって、観測値が得られる
    y[i, j] <- rnorm(
      n = 1,
      # ユーザーごとのランダム効果r を加える.
      mean = x[i] + r[j],
      sd = s_v
    )
  }
}


# データをまとめる
# 観測値・状態・時点番号をまとめた結果
sim_df <- y %>%
  cbind(state = x) %>%
  cbind(time = 1:100) %>%
  data.frame()

# 整然データの形式にする
sim_tidy <- sim_df %>%
  gather(
    key = "name",
    value = "sales",
    factor_key = TRUE,
    -time
  )

# Aさんは常に売り上げが低めで、Cさんは逆に売り上げが常に高めとなっている
# ユーザーの個性を認めつつ、全体の傾向を調べるのが目的
ggplot(data = sim_tidy) +
  ggtitle("シミュレーションにより生成された売り上げデータ（モデル１）") +
  geom_line(aes(x = time, y = sales, color = name)) +
  gghighlight(name == "state" |
    name == "A" | name == "C", use_group_by = F)


data_list_1 <- list(
  y = y,
  T = N,
  user_num = user_num
)
multi_1 <- stan(
  file = "./Documents/dev/r/r/time_series/ssm-user-sales_1.stan",
  data = data_list_1,
  seed = 1,
  iter = 30000,
  warmup = 10000,
  thin = 10,
)

mcmc_rhat(rhat(multi_1)) # 収束を確認

# モデル1の推定結果
# 標準偏差等が同じような値になっているかどうか確認.
print(
  multi_1,
  par = c("s_w", "s_v", "s_r", "lp__"),
  probs = c(0.025, 0.5, 0.975)
)

# データ整形
# 推定した状態と、95%信頼区間を縄掛けで表示
stan_df_1 <- multi_1 %>%
  rstan::extract() %$% x %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(1:nrow(sim_df)) %>%
  data.frame()

colnames(stan_df_1) <- c("lwr", "fit", "upr", "time")
head(stan_df_1)

# 図示
ggplot(data = sim_tidy) +
  ggtitle("推定結果（モデル１）") +
  geom_line(aes(x = time, y = sales, color = name)) +
  gghighlight(name == "state", use_group_by = FALSE) +
  geom_line(
    data = stan_df_1,
    aes(x = time, y = fit), size = 1.2
  ) +
  geom_ribbon(
    data = stan_df_1,
    aes(x = time, ymin = lwr, ymax = upr), alpha = 0.3
  )


# 今回のような単純なモデルでは、10人の売り上げの平均値と、状態の推定値はほぼ同じになる
cbind(
  y_mean = apply(y, 1, mean),
  state = x,
  fit = stan_df_1[, "fit"]
) %>%
  ts() %>%
  autoplot(facet = F, main = "状態推定値と平均値の比較")
