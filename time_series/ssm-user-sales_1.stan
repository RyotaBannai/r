// 外部から与えられるデータ
data {
  int T; // データ取得期間の長さ
  int user_num; // ユーザー数
  matrix[T,user_num] y; // 観測値
}
// stan が推定する必要があるパラメタを全て定義
parameters {
  vector[T] x; // 状態
  vector[user_num] r ; // ユーザーごとのランダム値
  real<lower=0> s_w ; // 過程誤差の標準偏差
  real<lower=0> s_v;// 観測誤差の標準偏差
  real<lower=0> s_r ;// ランダム値の標準偏差 // ユーザーごとのランダム値を生成する際に使う
}
// 推定するパラメタを使って、観測値y を表現するモデルを構築する.
// stan はこれをもとに処理する
model {
  // サービスの品質の状態がRWすることを表現
  // x[1] ~ normal(0, s_w);
  for (i in 2:T) {
    x[i] ~ normal(x[i - 1], s_w);
  }
  // ランダム効果
  r ~ normal(0, s_r);
  // 観測方程式に従い、観測値が得られる.
  for (i in 1:T) {
    for (j in 1:user_num) {
      y[i, j] ~ normal(x[i] + r[j], s_v);
    }
  }
}