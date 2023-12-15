# 単回帰

N <- 100
b0 <- 5
b1 <- 3
x <- rnorm(n = N, mean = 0, sd = 1)
e <- rnorm(n = N, mean = 0, sd = 2)
y <- b0 + b1 * x + e

plot(x, y)

model <- lm(y ~ x) # b0,b1 を予測してくれる
new <- data.frame(x = seq(min(x), max(x), 0.1))
A <- predict(model, new, se.fit = T, interval = "confidence") # 推定平均の95% 推定区間付き
B <- predict(model, new, se.fit = T, interval = "prediction") # 推定データの95% 推定区間付き

lines(as.matrix(new), A$fit[, 1], col = "black")
lines(as.matrix(new), A$fit[, 2], col = "red")
lines(as.matrix(new), A$fit[, 3], col = "red")
lines(as.matrix(new), B$fit[, 2], col = "blue")
lines(as.matrix(new), B$fit[, 3], col = "blue")


# 信頼区間と推定区間を一から求める
# これらのlo,up がfit.se, の信頼区間、A$fit と一致することが確認できる
C <- predict(model, new, se.fit = T)
loC <- C$fit - C$se.fit * qt(0.975, C$df) # qt はt検定の確率点.
upC <- C$fit + C$se.fit * qt(0.975, C$df)
# 推定誤差.平均値のばらつきに、データのばらつきを加えるイメージ
loP <- C$fit - sqrt(C$se.fit^2 + C$residual.scale^2) * qt(0.975, C$df) # qt はt検定の確率点.
upP <- C$fit + sqrt(C$se.fit^2 + C$residual.scale^2) * qt(0.975, C$df)

# 上と各区間が同じになるのが確認できる
plot(x, y)
lines(as.matrix(new), as.matrix(C$fit), col = "black")
lines(as.matrix(new), as.matrix(loC), col = "red")
lines(as.matrix(new), as.matrix(upC), col = "red")
lines(as.matrix(new), as.matrix(loP), col = "blue")
lines(as.matrix(new), as.matrix(upP), col = "blue")
