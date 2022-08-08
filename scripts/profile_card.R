##### LOAD PACKAGES
library(dplyr)
library(twitteR)
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
library(devtools)
library(plyr)												
library(readr)
library(plotly)
library(rtweet)
library(paletteer)
library(packcircles)
source("scripts/data.R")

profile_data <- users_data(tail(gfg_data, 1))

name <- profile_data$name
id <- paste("@", profile_data$screen_name, sep = "")
bio <- profile_data$description

x <- profile_data$account_created_at %>% as.Date()
joined <- paste("Joined", months(x), year(x), sep = " ")

ntweet <- paste(format(round(profile_data$statuses_count / 1e3, 2), trim = TRUE), "K")
following <- profile_data$friends_count
followers <- paste(format(round(profile_data$followers_count / 1e6, 1), trim = TRUE), "M")

## User image icon and banner links
icon_img <- profile_data$profile_image_url %>% 
  str_remove_all("_normal") %>%
  image_read() %>% 
  image_resize("300x300") %>% 
  image_convert(format = "svg") %>%
  magick::coder_info()
# plot(as.raster(icon_img))

banner_img <- profile_data$profile_banner_url %>% 
  paste("/1500x500", sep = "") %>%
  image_read() %>%
  image_convert(format = "svg")

image_write(icon_img, path = "assets/icon_img.svg", format = "svg")
image_write(banner_img, path = "assets/banner_image.svg", format = "svg")


profile_card <- data.frame(
  "Label" = c("Name", "Username", "Description", "Date Joined", "Statuses", "Friend Count", "Follower Count"), 
  "Information" = c(name, id, bio, joined, ntweet, following, followers))

card_out <- kable(profile_card, col.names = NULL)%>%
  kable_styling(full_width = FALSE)


