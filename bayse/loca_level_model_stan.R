# 「過程誤差」も「観測誤差」も平均値が0の正規分布に従うホワイトノイズだと仮定.
# よって、「過程誤差」と「観測誤差」の分散の値が推定できれば、ローカルレベルモデルが推定できる
# 「状態」はランダムウォーク

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
    real muZero; #左端
    vector[n] mu; # 確率的レベル
    real<lower=0> sigmaV; # 観測誤差の大きさ
    real<lower=0> sigmaW; # 過程誤差の大きさ
  }
  # 3．データが生成されるモデルを記述するブロック
  # モデルを指定する=確率分布を指定すること
  model {
    # 状態方程式
    mu[1] ~ normal(muZero, sqrt(sigmaW));

    # 状態の遷移
    for (i in 2:n) {
      mu[i] ~ normal(mu[i-1], sqrt(sigmaW));
    }

    # 観測方程式
    for (i in 1:n) {
      # 「ナイルのデータがこの状態方程式から生される」と定義
      Nile[i] ~ normal(mu[i], sqrt(sigmaV));
    }
  }
"

NileData = list(Nile = as.numeric(Nile), n = length(Nile))

set.seed(1)
Nile_LocalLevelModel_1 <- stan(
  model_code = localLevelModel_1,
  data = NileData,
  iter = 30000,
  warmup = 1000,
  thin = 10,
  chains = 3
)

# 計算の過程を図示
traceplot(Nile_LocalLevelModel_1, pars = c("sigmaV", "sigmaW"))

