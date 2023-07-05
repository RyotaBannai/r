# 重回帰

set.seed(0)
N = 100
intercept = 5
b1 = 10
b2 = 5
x1 = sort(rnorm(n = N, mean = 0, sd = 2))
x2 = rnorm(n = N, mean = 0, sd = 2)
e = rnorm(n = N, mean = 0, sd = 3)
y = intercept + b1 * x1 + b2 * x2 +  e

plot(x, y)

model1 = lm(y ~ x1)
model2 = lm(y ~ x1 + x2)
# model3: 相互作用を入れる.
# 今回のデータには相互作用は入れていないため「誤ったモデル」ということになる.
model3 = lm(y ~ x1 * x2)
# 星　*　が 付いていれば「役に立っている変数」で、ついて無ければ使えない変数
summary(model3)
anova(model2, model3)#　モデル2とモデル3の比較
