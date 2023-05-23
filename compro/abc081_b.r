# ABC081B - Shift only
# https://atcoder.jp/contests/language-test-202301/tasks/abc081_b

# 3
# 8 12 40

library(magrittr)
library(purrr)

dat <- readLines("stdin", n = 2)
xs <- dat[2] %>%
  strsplit(" ") %>%
  unlist() %>%
  map(as.integer)


# https://stat.ethz.ch/pipermail/r-help/2012-January/300250.html
mi <- .Machine$integer.max
for (x in xs) {
  if (x == 0) {
    mi <- 0
    break
  }
  bi <- intToBits(x)
  for (i in 1:100) {
    # 小さいindex がbit 表現時の小さい位
    if (bi[i] == "01") {
      mi <- min(mi, i - 1)
    }
  }
}

ans <- sprintf("%s\n", mi)
cat(ans)
