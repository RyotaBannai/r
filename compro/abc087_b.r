# ABC087B - Coins
# https://atcoder.jp/contests/language-test-202301/tasks/abc087_b

# 2
# 2
# 2
# 100

# あなたは、500 円玉を A 枚、100 円玉を B 枚、50 円玉を C 枚

library(magrittr)

dat <- readLines("stdin", n = 4) %>%
  strsplit(" ") %>%
  as.numeric()
x <- dat[[4]]
count <- 0
for (i in 0:dat[[1]]) {
  for (j in 0:dat[[2]]) {
    for (k in 0:dat[[3]]) {
      ret <- 500 * i + 100 * j + 50 * k
      if (ret == x) {
        count <- count + 1
      }
    }
  }
}

ans <- sprintf("%s\n", count)
cat(ans)
