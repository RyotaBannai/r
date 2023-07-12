# データの平均値と分散をベイズ推定
require(rstan)
# 計算の並列化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

NileData = list(Nile = as.numeric(Nile), n = length(Nile))

set.seed(1)
NileModel_1 = stan(
  file = './Documents/dev/r/r/bayse/llm_no_state_change.stan',
  data = NileData,
  iter = 1500,
  warmup = 500,
  thin = 1,
  chains = 3
)

# 計算の過程を図示
traceplot(NileModel_1)
