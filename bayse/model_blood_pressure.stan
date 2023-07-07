# Rstudio上で、stanファイルで定義した変数は、
# stanファイル内でtab で補完することができる.
data{
  int N_Medicine; # 薬を投与した日数
  int N; # 予測期間の合計日数
  vector[N] bloodPressure;  # 収縮期血圧値（心臓が収縮する時と、膨張する時の２パターンある）
  vector[N] medicine;  # 投与した薬の量(単純にカプセル何個か？的な値)
}
parameters {
  real muZero; # 血圧の「状態」の初期値
  vector[N] mu; # 血圧の「状態」
  real<lower=0> sigmaV;  # 観測誤差の大きさ（単なるランダムウォーク）
  real<lower=0> sigmaW; # 過程誤差の大きさ
  real coefMedicineTrendZero; # 薬の効果のトレンドの初期値（どんどんと効き目がなくなってくる）
  # 薬の効果の初期値（初めどれくらい効き目があるか、もちろん初めはすごく大きい）
  real coefMedicineZero; 
  vector[N_Medicine] coefMedicineTrend; # 薬の効果のトレンド（単なるランダムウォーク）
  vector[N_Medicine] coefMedicine; # 薬の効果
  vector[N_Medicine] coefMedicineReal; # ノイズの入った後の薬の効果（効き目にもランダム性がある）
  real<lower=0> sigmaWcoef;  #　薬の係数の過程誤差の大きさ
  real<lower=0> sigmaWcoefTrend;  #　薬の係数のトレンドの過程誤差の大きさ
  real<lower=0> sigmaVcoef;  # 薬の係数の観測誤差の大きさ
}
model {
  # 状態方程式
  # 血圧の状態の推定
  # 左端から初年度の状態を推定
  mu[1] ~ normal(muZero, sigmaW/100);
  # 血圧の状態遷移
  for (i in 2:N) {
    mu[i] ~ normal(mu[i-1], sigmaW/100);
  }
  # 薬の係数のトレンド
  coefMedicineTrend[1] ~ normal(coefMedicineTrendZero, sigmaWcoefTrend/100);
  for (i in 2:N_Medicine) {
    coefMedicineTrend[i] ~ normal(coefMedicineTrend[i-1], sigmaWcoefTrend/100);
  }
  # 薬の係数（状態と実際の効き目）トレンドを初回から混ぜ込む
  coefMedicine[1] ~ normal(coefMedicineZero + coefMedicineTrend[1], sigmaWcoef/100);
  for (i in 2:N_Medicine) {
    # 係数のトレンドを前の状態にそのまま混ぜ込んでいるから和分過程（累積和）である点にも注目
    coefMedicine[i] ~ normal(coefMedicine[i-1]+ coefMedicineTrend[i], sigmaWcoef/100);
  }
  # 薬の効き目にランダム性を加える
  for (i in 1:N_Medicine) {
    coefMedicineReal[i] ~ normal(coefMedicine[i], sigmaVcoef/100);
  }
  
  
  # 観測方程式（薬なし）初めの10日だけ
  for (i in 1:10) {
    bloodPressure[i] ~ normal(mu[i], sigmaV/100); # 観測誤差を使っている点に注目
  }
  # 観測方程式（薬あり）11 以降は薬を投与
  for (i in 11:N) {
    # 観測誤差を使っている点に注目
    bloodPressure[i] ~ normal(mu[i] + coefMedicineReal[i-10] * medicine[i], sigmaV/100); 
  }
  
  # 標準偏差の値が軒並み100で割られている
  # これは、標準偏差がとても小さな値になることがあるため（例えばsigmaWcoefTrendは0.03）
  # あんまりにも絶対値が小さい値を推定するのはちょっと難儀なため補正
}







