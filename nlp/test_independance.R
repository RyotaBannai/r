library(dplyr)
library(magrittr)
library(purrr)
library(readxl)

wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

dat <- read_excel("./source/sentences.xlsx")
dat_tb <- dat %>% xtabs(~ Sex + Sent, data = .) # クロス表
chisq.test(dat_tb) # 独立性の検定―カイ二乗検定
# > X-squared = 0.64838, df = 1, p-value = 0.4207
