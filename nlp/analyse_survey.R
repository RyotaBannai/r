library(dplyr)
library(magrittr)
library(purrr)
library(RMeCab)
library(stringr)
library(FactoMineR)
library(factoextra)

wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

okinawa <- read.csv("./source/H18koe.csv", stringsAsFactors = TRUE)
okinawa %>%
  select(Region:Satis) %>%
  summary() # String 型のカラムをfactor にすればsummary を適用できる

# <NA> が含まれている行をチェック.
# okinawa[rowSums(is.na(okinawa))!=0,]
# okinawa[okinawa %>% apply(1, anyNA),]


okinawa %<>% select(-Region) %>% na.omit()
# check again if df has no NA data

okinawa %>% xtabs(~ Sex + Satis, data = .)
# カラムを二つ選んで、table にかけてもクロス集計できる
# okinawa %>% select(Sex, Satis) %>% table


get_unique_factor <- function(colname) {
  # use_series は$のエイリアス.
  return(okinawa[[colname]] %>%
    factor() %>% # もしカラムがfactor じゃないならfactor へ変換.
    levels())
}

age_l <- get_unique_factor("Age")
age_l <- age_l[-1]
sex_l <- get_unique_factor("Sex")

expand.grid(a = age_l, s = sex_l) %>%
  apply(MARGIN = 1, FUN = \(x) filter(okinawa, Age == x[1], Sex == x[2]) %>%
    {
      opinion <- use_series(data = ., Opinion) %>% as.character()
      # 全角数字から半角へ変換. # 1 は事前に落としているから2 から始める
      # library(stringi)
      # stri_trans_nfkc("３０") でも感じでもok
      # https://pediatricsurgery.hatenadiary.jp/entry/2017/10/12/105242
      half_num <- (2:7)[age_l == x[1]]
      filename <- paste0(
        "./source/okinawa/",
        ifelse(x[2] == "女性", "F", "M"),
        half_num,
        "0.txt"
      )
      writeLines(text = opinion, con = filename)
    })

# stopifnot: Ensure the Truth of R Expressions
# identical: Test Objects for Exact Equality



fm <- docDF("./source/okinawa", type = 1, pos = c("名詞", "動詞", "形容詞"))
fm2 <- fm %>% filter(POS2 %in% c("一般", "固有名詞", "自立"))
fm2 %<>% filter(!TERM %in% c("ある", "いう", "いる", "する", "できる", "なる", "思う"))

# 回答全体を通して、7回以上出現した形態素だけに絞る
fm2$SUMS <- rowSums(fm2[, -(1:3)])
fm3 <- fm2 %>% filter(SUMS >= 7)
fm4 <- fm3 %>% select(matches("[FM]\\d\\d"))
colnames(fm4) <- str_extract(colnames(fm4), "[FM]\\d\\d")
rownames(fm4) <- fm3$TERM

fm4ca <- CA(fm4, graph = FALSE)
# ggplot2 ベースのバイプロットを描く.
fviz_ca_biplot(fm4ca)


# 簡易的な対応分析
# デフォルトでは、bycol だから、byrow=TRUE にする.
# bycol だと入力された要素が、カラム方向に配置される. (今回は、要素を順に行方向に配置したい)
dat <- matrix(c(1, 2, 0, 0, 0, 2, 6, 0, 0, 1, 2, 2, 0, 0, 0, 2),
  ncol = 4, byrow = TRUE
)
ed <- c("中卒", "高校中退", "高卒", "大卒")
gen <- c("F", "M")
labels_ <- expand.grid(ed, gen) %>%
  apply(MARGIN = 1, FUN = \(row) paste(row[1], row[2]))
colnames(dat) <- labels_[1:4]
rownames(dat) <- labels_[5:8]

dat_ca <- CA(dat, graph = FALSE)
fviz_ca_biplot(dat_ca)

# MASS でも良い. ただし、dplyr の関数と衝突するから、namespace をつけて呼び出す.
dat_ca2 <- MASS::corresp(dat, nf = 2)
biplot(dat_ca2)
