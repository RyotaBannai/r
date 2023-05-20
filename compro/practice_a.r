# PracticeA - Welcome to AtCoder
# https://atcoder.jp/contests/language-test-202301/tasks/practice_1

# 1
# 2 3
# test

library(magrittr)

dat <- readLines("stdin")
ret <- dat[1] %>% as.integer()
ret <- ret + dat[2] %>%
  strsplit(" ") %>%
  unlist() %>%
  as.integer() %>%
  sum()

ans <- sprintf("%d\n%s\n", ret, dat[3])
cat(ans)
