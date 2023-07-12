# 「過程誤差」も「観測誤差」も平均値が0の正規分布に従うホワイトノイズだと仮定.
# よって、「過程誤差」と「観測誤差」の分散の値が推定できれば、ローカルレベルモデルが推定できる
# 「状態」はランダムウォーク

# データの平均値と分散をベイズ推定
require(rstan)
# 計算の並列化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

NileData = list(Nile = as.numeric(Nile), n = length(Nile))

set.seed(1)
Nile_LocalLevelModel_1 <- stan(
  file = './Documents/dev/r/r/bayse/local_level_model.stan',
  data = NileData,
  iter = 30000,
  warmup = 1000,
  thin = 10,
  chains = 3
)

# 計算の過程を図示
traceplot(Nile_LocalLevelModel_1, pars = c("sigmaV", "sigmaW"))

