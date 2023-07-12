# 統計モデルの記述
# モデルの作成
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