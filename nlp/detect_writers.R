#' 書き手の判別
#' テキストマイニングによって書き手の癖を抽出し、テキストの書き手を見分ける
#' 書き手の判別にはさまざまな方法があるが、ここではバイグラムを使う
#'
#' 使用するテキストについて
#' - 新字新仮名で書かれているテキストを使う
#' - 作品の時代背景が異なると、出現するタームの種類や頻度が極端に異なることがあるため、明治時代期以前の作品は避けるなど工夫
#' - テキストの長さをある程度統一させる

library(RMeCab)
library(dplyr)
library(stringr)
library(magrittr)
library(plotly)
library(ggdendro)
library(ggfortify)

wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

res <- docNgram("./source/writers", type = 0)
# 各作家が書いた作品のバイグラムから分類できることを確認.
res %>%
  t() %>%
  dist() %>%
  hclust("ward.D2") %>%
  ggdendrogram()

# 助詞と読点の組み合わせに作家の特徴が現れるから、
# バイグラムよりそれらを抽出する.
res2 <- res[rownames(res) %in%
  c(
    "[と-、]", "[て-、]", "[は-、]", "[が-、]",
    "[で-、]", "[に-、]", "[ら-、]", "[も-、]"
  ), ]

res2_pc <- res2 %>%
  t() %>%
  princomp()
summary(res2_pc)

rownames(res2_pc$scores) %<>%
  str_extract("[a-z]+") %>%
  paste0(1:8)

autoplot(res2_pc,
  label = TRUE,
  label.size = 8,
  loadings = TRUE,
  lodings.label = TRUE,
  loadings.label.size = 12,
  lodings.label.family = "JP1"
)
