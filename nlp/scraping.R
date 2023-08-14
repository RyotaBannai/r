# ブクログから「スティーブ・ジョブズ 驚異のプレゼン」
# のレビューを取得して解析対象のデータソースにする

# reviews.txt を保存する際に、working directory が想定通りになっているか.
setwd("~/Documents/dev/r/r/nlp/")

library(dplyr)
library(rvest)
library(RMeCab)
library(igraph)

dat_path <- "./source/reviews.txt"

create_data <- function() {
  review_path <- "https://booklog.jp/item/1/482224816X?perpage=30&rating=0&is_read_more=1&sort=1"
  reviews <- read_html(review_path)
  reviews %>%
    html_elements(".review-txt") %>%
    html_text() %>%
    writeLines(dat_path)
}

create_data()

# 頻度表
revi <- docDF(dat_path, type = 1, pos = c("名詞", "形容詞", "動詞"))
revi2 <- revi %>%
  # filter(POS1 == "名詞",
  filter(is_in(POS2, c("一般", "固有名詞", "自立"))) %>% # 「自立」は動詞、形容詞について限定
  rename(FREQ = reviews.txt) %>%
  arrange(desc(FREQ))

# バイグラム
bigram <-
  docDF(
    dat_path,
    type = 1,
    nDF = 1,
    N = 2,
    pos = c("名詞", "形容詞", "動詞")
  ) %>%
  rename(FREQ = reviews.txt)

bigram2 <- bigram %>%
  select(N1, N2, FREQ) %>%
  filter(FREQ > 2) %>%
  arrange(desc(FREQ))

par(mar = c(0, 0, 0, 0) + .1)
bigram_n <- graph_from_data_frame(bigram2)
tkplot(bigram_n, vertex.color = "SkyBlue", vertex.size = 4, width = 1000)
