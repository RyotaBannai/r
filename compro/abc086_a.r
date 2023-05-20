# ABC086A - Product
# https://atcoder.jp/contests/language-test-202301/tasks/abc086_a

# 3 4

library(magrittr)

dat <- readLines("stdin")
x <- dat[1] %>%
  strsplit(" ") %>%
  unlist() %>%
  as.integer() %>%
  Reduce(f = "*", init = 1)

x <- if (x %% 2 == 0) "Even" else "Odd"
ans <- sprintf("%s\n", x)
cat(ans)
