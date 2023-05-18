library(magrittr)
# 1
# 2 3
# test

a <- readLines("stdin", n = 1) %>% as.integer()
bc <- readLines("stdin", n = 1) %>%
  strsplit(" ") %>%
  unlist() %>%
  as.integer()
s <- readLines("stdin", n = 1)

ans <- sprintf("%d\n%s\n", a[1] + bc[1] + bc[2], s)
cat(ans)
