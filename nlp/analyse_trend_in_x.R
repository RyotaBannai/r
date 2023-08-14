#' Twitter API を使った、トレンド分析.
#'
# install.packages(
#   c("twitteR", "bit64", "rjson", "DBI", "httr", "base64enc"),
#   dependencies = TRUE,
# )

library(RMeCab)
library(dplyr)
library(stringr)
library(magrittr)
# library(twitteR)
# library(RTwitterV2)
library(httr)
library(jsonlite)



wd <- "~/Documents/dev/r/r/nlp/"
setwd(wd)

# Twitter API を使うため環境変数を読み込む
readRenviron(".env")
consumer_key <- Sys.getenv("XAPIKey")
consumer_secret <- Sys.getenv("XAPIKeySecret")
access_token <- Sys.getenv("XAccessToken")
access_token_secret <- Sys.getenv("XAccessTokenSecret")
bearer_token <- Sys.getenv("BearerToken")

headers <- c(`Authorization` = sprintf("Bearer %s", bearer_token))
params <- list(
  `user.fields` = "description",
  `expansions` = "pinned_tweet_id"
)

url_handle <- sprintf("https://api.twitter.com/2/users/me")

response <-
  httr::GET(
    url = url_handle,
    httr::add_headers(.headers = headers),
    query = params
  )
obj <- httr::content(response, as = "text")
print(obj)
