# ABC081A - Placing Marbles
# https://atcoder.jp/contests/language-test-202301/tasks/abc081_a

# 101

library(magrittr)

dat <- readLines("stdin")
xs <- dat[1] %>%
  strsplit("") %>%
  unlist()

count <- 0
for (x in xs) {
  if (x == "1") {
    count <- count + 1
  }
}

ans <- sprintf("%s\n", count)
cat(ans)
