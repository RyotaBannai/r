#' 総理大臣の所信表明コーパス
#' https://github.com/yuukimiyo/GeneralPolicySpeechOfPrimeMinisterOfJapan

#' 総理大臣の所信表明演説は「その時代の政治的経済的課題を反映しているはず」であり、
#' 演説の内容を分類すると、時代ごとにクラスターを形成するのではないかと仮定して分析.
library(RMeCab)
library(dplyr)
library(stringr)
library(magrittr)
library(ggdendro)
library(rgl)
library(plotly)


wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

prime <- docMatrix2(
  "./source/GeneralPolicySpeechOfPrimeMinisterOfJapan-master/longfilename/utf8",
  pos = c("名詞", "形容詞", "動詞"),
  weight = "tf*idf*norm"
)

# ファイル名が冗長だから簡潔な形式に変換
colnames(prime) %<>% str_replace("_general-policy-speech.txt", "")
#  20130128_183 -> 2013_183
colnames(prime) %<>% str_replace("(\\d{4})\\d{4}_(\\d{2,3})", "\\1_\\2")
# (prime %>%
#   head(1))[prime %>%
#   head(1) %>%
#   sapply(FUN = \(x) x != 0)]


# R でクラスター分析を行うには、行に分類対象である文章を並べる必要があるためt() する.
# クラスターを求めるためにウォード法を用いる.
hc <- prime %>%
  t() %>%
  dist() %>%
  hclust("ward.D2")
ggdendrogram(hc, rotate = TRUE)

# LSI
prime.svd <- svd(prime)
prime2 <- t(prime.svd$u[, 1:3]) %*% prime
colnames(prime2) <- prime2 %>%
  colnames() %>%
  str_extract("\\d{4}_\\d{2,3}")

cols <- prime2 %>%
  colnames() %>%
  str_extract("\\d{3}")

# rgl.open()
# par3d(text = 5)
# # 座標を色分け
# rgl.lines(c(-1, 1), 0, 0, color = "gold")
# rgl.lines(0, c(-1, 1), 0, color = "gray")
# rgl.lines(0, 0, c(-1, 1), color = "black")
# rgl.bbox(color = "blue", emission = "green")
# rgl.texts(prime2[1, ], prime2[2, ], prime2[3, ], colnames(prime2), color = cols)
# rgl.close()

prime_df <- prime2 %>%
  t() %>%
  as.data.frame()

colnames(prime_df) <- c("x", "y", "z")
prime %<>% cbind(cols)
fig <- plot_ly(prime_df, x = ~x, y = ~y, z = ~z, color = ~cols)
fig <- fig %>% add_markers()
fig
