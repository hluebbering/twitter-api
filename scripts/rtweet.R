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
# library(academictwitteR)
library(rtweetXtras)
# install_version("rtweet", version = "0.7.0")



api_key1 <- "AD6QMEvWQGztogOmoVHEkWLoE"
api_key_secret1 <- "BsihaN82bsfHkZxnAD2a9s4m0amKewB4hFdf94erAXQyLhoWjQ"
access_token1 <- "1357218984798609409-WeWWKQ9ucDaJTCl8DH4X0sohI5uXVc"
access_token_secret1 <- "PcWLNnAJwFTKM5WrIWomaDHuadaW35JwRRcSPartYCdh6"

bearer_toke <- "AAAAAAAAAAAAAAAAAAAAAA9AbwEAAAAA6zhbH%2Bbhnw1ytRVAl9rQhEypn0A%3Duhy92utPtyr1cVdgQgHR6wCf4nKYgyVVGKXJOq3GV7zzeh3JAH"


client_id <- "YXY3Y2xDa3czM3FxVm83aEZ6YXM6MTpjaQ"
client_secret <- "v-ow4p7dYZws_VnJELslgrzlkcrhq_O0qVjetCLM9U96XjcVW7"

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
# musktweets8 <-
#   search_fullarchive(
#     paste0("from:", users, collapse = " OR "),
#     n = 2000,
#     env_name = "devenviroment",
#     fromDate = "202206210000",
#     toDate = "202208050000")

# musktweets5 <-
#   search_fullarchive(
#     paste0("from:", users, collapse = " OR "),
#     n = 9000,
#     env_name = "devv",
#     fromDate = "201001010000",
#     toDate = "201201010000")

# save_as_csv(musktweets8, file_name = "data/musktweets202208.csv")

