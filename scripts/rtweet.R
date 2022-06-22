library(dplyr)
library(twitteR)
library(rtweet)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(scales)
library(tidyr)
library(ggplot2)
library(tidytext)
library(quanteda)
library(hrbrthemes)
library(httr)

api_key1 = "HqET15WxmS965qTH2q5u8tJtI"
api_key_secret1 = "uAH8a2MNgRUT043js8SuhiAW1VFDuOFq5KjMuIUwa7rJtsEF2Q"
access_token1 = "1357218984798609409-h8nBD6Hr8ulTidM55CEqq5n95Haa2e"
access_token_secret1 = "2UyY24reVECUidf8x97o9mrxkA7EGn6y8z8tyCk8Ie9UB"

bearer_toke = "AAAAAAAAAAAAAAAAAAAAAA9AbwEAAAAAvE6p2MrtLewTfUKMvN8f45kBgmg%3Dq0KVRZOG9uA3Ahhp41Hkirpg7pS0HHsoD7H3WTILajhptiyk8e"
client_id = "YXY3Y2xDa3czM3FxVm83aEZ6YXM6MTpjaQ"
client_secret = "tNmcI6uGBfbi8zV9AUKmzeOZbZ-B9jKlkDnaOKPiKMop61r5j6"

twitter_token <- create_token(
  app = "LuebberingApp",
  consumer_key = api_key1,
  consumer_secret = api_key_secret1,
  access_token = access_token1,
  access_secret = access_token_secret1,
  set_renv = TRUE
)
use_oauth_token(twitter_token)


# users <- "elonmusk"
# 
# musktweets7 <-
#   search_fullarchive(
#     paste0("from:", users, collapse = " OR "),
#     n = 2000,
#     env_name = "devv",
#     fromDate = "202205272255",
#     toDate = "202206210000")

# musktweets5 <-
#   search_fullarchive(
#     paste0("from:", users, collapse = " OR "),
#     n = 9000,
#     env_name = "devv",
#     fromDate = "201001010000",
#     toDate = "201201010000")

# save_as_csv(musktweets7, file_name = "data/musktweets202206.csv")

