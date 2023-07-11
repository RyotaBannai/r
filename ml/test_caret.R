library(caret)
# 並列化演算を行う
library(doParallel)
cl = makePSOCKcluster(4)
registerDoParallel(cl)


#----------------------------------------------------------------------------------
# caret 回帰
#----------------------------------------------------------------------------------

# シミュレーションでデータを作る:

set.seed(0)
N = 1500
x1 = runif(N, min = -5, max = 5)
x2 = runif(N, min = -10, max = 10)
x3 = runif(N, min = 0, max = 10)

y = (sin(x1 *  pi / 2) * 4 + x1 * 0.5) * x2 + x3 * 2 + (x3 ^ 2) * 0.4 + rnorm(N, mean = 0, sd = 1)

# 図示
par(mar = c(1, 1, 1, 1)) # This fixes「Error in plot.new() : figure margins too large」
# > par("mar")
# [1] 5.1 4.1 4.1 2.1
par(mfrow = c(3, 1))
plot(y ~ x1, main = "x1")
plot(y ~ x2, main = "x2")
plot(y ~ x3, main = "x3")
par(mfrow = c(1, 1))


library(scatterplot3d)
scatterplot3d(x1, x2, y)


# data.frameにまとめる。
dataRegression = data.frame(y = y,
                            x1 = x1,
                            x2 = x2,
                            x3 = x3)

# テスト用と学習用にデータを分ける
dataRegressionTrain = dataRegression[1:1000,]
dataRegressionTest = dataRegression[1001:1500,]

# 重回帰
# formulaに「y ~ (.)^2」と指定して2次の交互作用まですべて入れている
modelLm = lm(y ~ (.) ^ 2,
             data = dataRegressionTrain)
modelLm = step(modelLm) #  モデルを作った後で変数選択


set.seed(0)
modelNnetReg = train(
  # 第一引数： 説明変数と応答変数の関係を定義.
  # ここでは、y を教師データとして、それ以外の変数と相互作用を入力に使ってモデル構築する
  y ~ (.) ^ 2,
  data = dataRegression,
  method = 'nnet',
  preProcess = c("center", 'scale'),
  trControl = trainControl(method = 'cv'),
  # ハイパラのチューニングはmethod によって異なる
  tuneGrid = expand.grid(size = c(1:10), decay = seq(0.1, 1, 0.1)),
  linout = TRUE # ここをFALSEにすると分類. 回帰ならTRUE. この引数はnnetにのみ必要?
)


# ランダムフォレストによる予測
# randomForestパッケージを使う。
modelRFReg = train(
  y ~ (.) ^ 2,
  data = dataRegressionTrain,
  method = "rf",
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# rangerパッケージを使う。
modelRangerReg = train(
  y ~ (.) ^ 2,
  data = dataRegressionTrain,
  method = "ranger",
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# Rboristパッケージを使う。
modelRboristReg = train(
  y ~ (.) ^ 2,
  data = dataRegressionTrain,
  method = "Rborist",
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# テストデータで予測する
predLm = predict(modelLm, dataRegressionTest)
predNnet = predict(modelNnetReg, dataRegressionTest)
predRF = predict(modelRFReg, dataRegressionTest)
predRanger = predict(modelRangerReg, dataRegressionTest)
predRborist = predict(modelRboristReg, dataRegressionTest)


# RMSE
sqrt(sum((dataRegressionTest$y - predLm) ^ 2) / 500)
sqrt(sum((dataRegressionTest$y - predNnet) ^ 2) / 500)
sqrt(sum((dataRegressionTest$y - predRF) ^ 2) / 500)
sqrt(sum((dataRegressionTest$y - predRanger) ^ 2) / 500)
sqrt(sum((dataRegressionTest$y - predRborist) ^ 2) / 500)



#----------------------------------------------------------------------------------
# caret 分類
#----------------------------------------------------------------------------------


# アヤメのデータを使う
# アヤメの種類を判別する
head(iris)
# 3の倍数のものをテストデータ、それ以外をトレーニングデータ
indexIris = which(1:nrow(iris) %% 3 == 0) 
irisTrain = iris[-indexIris,]
irisTest = iris[indexIris,]

modelNnetCls = train(
  Species ~ (.) ^ 2,
  data = irisTrain,
  method = 'nnet',
  preProcess = c("center", 'scale'),
  trControl = trainControl(method = 'cv'),
  tuneGrid = expand.grid(size = c(1:10), decay = seq(0.1, 1, 0.1)),
  linout = F
)

modelRangerCls = train(
  Species ~ (.) ^ 2,
  data = irisTrain,
  method = "ranger",
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)

# 予測
predIrisNnet <- predict(modelNnetCls, irisTest)
predIrisRanger <- predict(modelRangerCls, irisTest)

confusionMatrix(data = predIrisNnet, irisTest$Species)
confusionMatrix(data = predIrisRanger, irisTest$Species)

