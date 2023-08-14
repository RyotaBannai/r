library(RMeCab)
library(dplyr)
library(stringr)
library(magrittr)
library(lda)
library(topicmodels)
library(tm)
library(tidyr)
library(ggplot2)


wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

prime <- docDF(
  "./source/GeneralPolicySpeechOfPrimeMinisterOfJapan-master/longfilename/utf8",
  pos = c("名詞", "形容詞"),
  minFreq = 3,
  type = 1
)

prime2 <- prime %>% filter(POS2 %in% c("一般", "自立"))

# 重複してる形態素があるか？
prime2$TERM %>%
  duplicated() %>%
  which()

prime3 <- prime2 %>% select(-c(TERM:POS2))
rownames(prime3) <- prime2$TERM
# ファイル名が冗長だから簡潔な形式に変換
colnames(prime) %<>% str_replace("_general-policy-speech.txt", "")
#  20130128_183 -> 2013_183
colnames(prime) %<>% str_replace("(\\d{4})\\d{4}_(\\d{2,3})", "\\1_\\2")

prime3a <- prime3 %>%
  t() %>%
  as.DocumentTermMatrix(weighting = weightTf)

# データの状況を確認
prime3a %>% inspect()

# トピックモデルを実行.
k <- 5 # トピック数
res1 <- prime3a %>% LDA(k)
# 推定されたトピック
terms(res1)

# トピックごとの単語の出現確率、テキストごとのトピックの比重
posterior(res1)[[1]][1:5, 1:10] # 行がトピック, 列が単語
posterior(res1)[[2]]

prime4 <- dtm2ldaformat(prime3a)
set.seed(123)
result <- lda.collapsed.gibbs.sampler(
  prime4$documents,
  K = k,
  prime4$vocab, 25, 0.1, 0.1,
  compute.log.likelihood = TRUE
)

# トピックごとに出現スコアが高いキーワードを１０個出力
top.topic.words(result$topics, 10, by.score = TRUE)
result$document_sums

prime5 <- rownames(prime3a) %>% str_subset("koizumi|hatoyama|noda|abe")
prime6 <- rownames(prime3a) %>%
  str_detect("koizumi|hatoyama|noda|abe") %>%
  which()
cbind(prime6, prime5)

# トピックの割合を計算
topic.proportions <- t(result$document_sums) / colSums(result$document_sums) # 割り算を行単位に適用
ministers <- topic.proportions[c(64, 74, 77, 80), ]
ministers %>% rowSums()

ministers_df <- ministers %>%
  as.data.frame() %>%
  set_names(paste0("topic", 1:5)) %>%
  mutate(num = paste0("No.", c(64, 74, 77, 80)))

ministers_df2 <- ministers_df %>% gather(key = topic, value = props, -num)
ministers_df2 %>% ggplot(aes(x = topic, y = props, fill = num)) +
  geom_bar(stat = "identity") +
  facet_wrap(~num)
