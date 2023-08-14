library(RMeCab)
library(dplyr)
library(wordcloud)

# run `Aozora('https://www.aozora.gr.jp/cards/000081/files/43754_ruby_17594.zip')` beforehand
wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

# 形態素解析
miyaz <- docDF("./NORUBY/chumonno_oi_ryoriten2.txt", type = 1)
miyaz2 <- miyaz %>%
  rename(FREQ = chumonno_oi_ryoriten2.txt) %>%
  filter(
    POS1 %in% c("名詞", "形容詞"),
    POS2 %in% c("一般", "自立")
  )
wordcloud(miyaz2$TERM,
  miyaz2$FREQ,
  min.freq = 3, scale = c(6, 1), family = "JP1",
  colors = brewer.pal(8, "Dark2")
)
