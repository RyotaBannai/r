# データの平均値と分散をベイズ推定
require(rstan)
# 計算の並列化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 統計モデルの記述
# モデルの作成
localLevelModel_1 = "
  # 1．データを指定するブロック
  data {
    int n;
    vector[n] Nile;
  }
  # 2．推定するパラメタを指定するブロック
  parameters {
    real mu;              # 確定的レベル（データの平均値）
    real<lower=0> sigmaV; # 観測誤差の大きさ（「0より小さな値はとらない」と指示）
  }
  # 3．データが生成されるモデルを記述するブロック
  # モデルを指定する=確率分布を指定すること
  model {
    for(i in 1:n) {
      Nile[i] ~ normal(mu, sqrt(sigmaV));
    }
  }
"

NileData = list(Nile = as.numeric(Nile), n = length(Nile))

set.seed(1)
NileModel_1 = stan(
  model_code = localLevelModel_1,
  data = NileData,
  iter = 1500,
  warmup = 500,
  thin = 1,
  chains = 3
)

# 計算の過程を図示
traceplot(NileModel_1)
