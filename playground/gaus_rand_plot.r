# check value in vector
# v <- c('a','b','c','e')
# 'b' %in% v
## returns TRUE

# match('b',v)
## returns the first location of 'b', in this case: 2

# Box-Muller Transform
n <- 1000
z1 <- rep(NA, n)
z2 <- rep(NA, n)
for (sim in 1:n) {
  u1 <- runif(1) # max=1 と指定しなくても、0<x<1の範囲に収まる.
  u2 <- runif(1)
  r <- sqrt(-2 * log(u1)) # log_e
  theta <- 2 * pi * u2
  z1[sim] <- r * cos(theta)
  z2[sim] <- r * sin(theta)
}
label <- rep("z1", n)
df <- as.data.frame(cbind(z1, label)) # cbin dfの行を作る. column bin

z1 <- rnorm(n) # 上書き
label <- rep("rvals", n) # 上書き
df2 <- as.data.frame(cbind(z1, label))

df3 <- rbind(df, df2) # row bind
df3$z1 <-as.numeric(df3$z1) # coerce to double precision

library("ggplot2")
ggplot(df3, aes(x = z1, color = label, fill = label)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  theme(legend.position = "top") +
  theme_classic()
