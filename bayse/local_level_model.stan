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