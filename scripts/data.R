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


#### SAVING CSV DATA FILES ####

gfg_data <-
  list.files(path = "data",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
 
gfg_data <- gfg_data %>%
  arrange(as.Date(gfg_data$created_at))
# write.csv(gfg_data, file = "musktweets1022.csv")
# gfg_data <- read.csv("musktweets1222.csv")



#### TWEETS DATAFRAME ####

dfpreview <- gfg_data %>% arrange(desc(created_at)) %>%
  dplyr::select(screen_name, created_at, text, source, media_type, is_quote, 
                is_retweet, reply_to_screen_name,  mentions_screen_name, 
                hashtags, retweet_count, favorite_count)

kbl.preview <- dfpreview  %>% head(2) %>%
  kable(escape = T, col.names = c("user", "created", "text", "source", 
                      "media", "quote", "retweet", "reply to", 
                      "mentions", "hash tags", "retweet count", "fav count")) %>% 
  kable_styling(full_width = T, font_size = 9, html_font = "roboto", bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, background = "rgb(29, 155, 240)", font_size = 7.8, color = "white", extra_css = "text-transform: uppercase; font-weight: 400; letter-spacing: 0.5px; text-align: right; text-shadow: 1px 1px 1px rgb(0 0 0 / 20%);") %>%
  column_spec(1:12, extra_css = "font-weight: 300; text-align: right; font-family: roboto condensed !important; text-shadow: 1px 1px 0px #ffffff;") %>%
  column_spec(c(3,4, 9), width_max  = "0.7in", extra_css = "font-size: 7.8px;") %>% 
  column_spec(1, width_max  = "0.72in", extra_css = "font-weight: 400; font-size: 9.25px;") %>%
  column_spec(c(2, 11, 12), width_min  = "0.6in", width_max = "0.7in", extra_css = "font-weight: 400; color: hsl(227deg 12% 43%); font-size: 9.25px;") %>%
  column_spec(8, extra_css = "font-weight: 400; color: #61677c; font-size: 8.25px;") %>% 
  scroll_box(width = "100%", extra_css = "overflow-x: overlay !important; border: none !important;")


#### RETWEETS DATAFRAME ####

df.retweet <- gfg_data %>%
  dplyr::filter(is_retweet==TRUE) %>% 
  dplyr::select(c(retweet_status_id, retweet_text, retweet_created_at, 
           retweet_source, retweet_favorite_count, retweet_retweet_count, 
           retweet_user_id, retweet_screen_name, retweet_name, 
           retweet_followers_count, retweet_friends_count, 
           retweet_statuses_count, retweet_location, 
           retweet_description, retweet_verified)) 

kbl.retweet <- kable(head(df.retweet, 2), escape = F) %>% 
  kable_styling(full_width = T, font_size = 9, html_font = "roboto", 
                bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, background = "rgb(29, 155, 240)", font_size = 7.8, color = "white", 
           extra_css = "text-transform: uppercase; font-weight: 400; letter-spacing: 0.5px; text-align: right; text-shadow: 1px 1px 1px rgb(0 0 0 / 20%);") %>%
  column_spec(1:15, extra_css = "font-weight: 300; text-align: right; font-family: roboto condensed !important; text-shadow: 1px 1px 0px #ffffff;") %>%
  column_spec(1, width_max  = "0.72in", extra_css = "font-weight: 400; font-size: 9.25px;") %>%
  scroll_box(width = "100%", extra_css = "overflow-x: overlay !important; border: none !important;")



#### TWEET WORDS DATAFRAME ####

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
# tweet_words <- gfg_data %>%
#   dplyr::select(status_id, source, text, created_at) %>%
#   dplyr::filter(!str_detect(text, '^"')) %>%
#   dplyr::mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
#   unnest_tokens(word, text, token = "regex", pattern = reg) %>%
#   dplyr::filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))


tweet_words <- gfg_data %>%
  dplyr::mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
                text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
                text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  dplyr::filter(!word %in% stop_words$word,
                !word %in% str_remove_all(stop_words$word, "'"),
                str_detect(word, "[a-z]"),
                !str_detect(word, "^#"),         
                !str_detect(word, "@\\S+"))

df.words <- tweet_words %>%
  dplyr::count(word, sort = TRUE) %>% head(20)

kbl.words <-  kable(df.words) %>% 
  kable_styling(full_width = F, font_size = 9, html_font = "roboto", bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, background = "rgb(29, 155, 240)", font_size = 7.8, color = "white", 
           extra_css = "text-transform: uppercase; font-weight: 400; letter-spacing: 0.5px; text-align: right; text-shadow: 1px 1px 1px rgb(0 0 0 / 20%);") %>%
  column_spec(1:2, extra_css = "font-weight: 300; text-align: right; font-family: roboto condensed !important; text-shadow: 1px 1px 0px #ffffff;")

plot.words <- tweet_words %>%
  dplyr::count(word, sort = TRUE) %>%
  head(20) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip() +
  theme_ipsum_rc(base_size = 10) +
  theme(legend.position = "none")



#### TWEET QUOTES DATAFRAME ####

df_quote <- gfg_data %>%
  filter(is_quote==TRUE) %>% 
  select(c(quoted_status_id, quote_count, quoted_text, quoted_created_at, 
           quoted_source, quoted_favorite_count, quoted_retweet_count, 
           quoted_user_id, quoted_screen_name, quoted_name, 
           quoted_followers_count, quoted_friends_count, quoted_statuses_count, 
           quoted_location, quoted_description, quoted_verified))

kbl.quote <- head(df_quote) %>% 
  kable(escape = FALSE) %>%
  kable_styling(full_width = T, font_size = 9, html_font = "roboto", bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, background = "rgb(29, 155, 240)", font_size = 7.8, color = "white", extra_css = "text-transform: uppercase; font-weight: 400; letter-spacing: 0.5px; text-align: right; text-shadow: 1px 1px 1px rgb(0 0 0 / 20%);") %>%
  column_spec(1:16, extra_css = "font-weight: 300; text-align: right; font-family: roboto condensed !important; text-shadow: 1px 1px 0px #ffffff;") %>%
  column_spec(1, width_max  = "0.72in", extra_css = "font-weight: 400; font-size: 9.25px;") %>%
  scroll_box(width = "100%", extra_css = "overflow-x: overlay !important; border: none !important;")



#### TIME/DATE/WEEKDAY TWEET ANALYSIS ####

y <- as.POSIXlt(gfg_data$created_at)

dftweets <- gfg_data %>%
  dplyr::select(status_id, created_at, text)
dftweets$date <- format(y, format = "%m/%d/%Y")
dftweets$weekday <- weekdays(y)
dftweets$time <- format(y, format = "%H:%M:%S")
dftweets$week <-
  factor(
    dftweets$weekday,
    levels = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
  )



#### TWEET TYPES (RETWEET/QUOTE/MENTIONS/ETC.) ####

retweets <- nrow(gfg_data[gfg_data$is_retweet == 'TRUE',])
quotes <- nrow(gfg_data[gfg_data$is_quote == 'TRUE',])
normal <- nrow(gfg_data[gfg_data$is_quote == 'FALSE' &
                          gfg_data$is_retweet == 'FALSE' &
                          gfg_data$reply_to_user_id == 'NA',])
mentions <- nrow(data.frame(na.omit(gfg_data$mentions_user_id)))
replies <- nrow(data.frame(na.omit(gfg_data$reply_to_status_id)))


tweet.types <- data.frame(
  type = c("retweet", "quote", "regular", "reply"),
  n = c(retweets, quotes, normal, replies)) %>%
  #dplyr::mutate(type = factor(type, levels = c("retweet", "quote", "regular", "reply"))) %>%
  dplyr::arrange(dplyr::desc(n))


#### TOP MENTIONS & HASHTAGS ####

dfMentions <- gfg_data$mentions_screen_name %>%
  strsplit(" ") %>% unlist(use.names = FALSE) %>% tolower %>%
  table() %>% sort(decreasing = TRUE) # %>% head(20)

dfHashtags <- gfg_data$hashtags %>%
  strsplit(" ") %>% unlist(use.names = FALSE) %>% tolower %>%
  table() %>% sort(decreasing = TRUE) # %>% head(20)


#### SENTIMENT ANALYSIS PART 1 ####

round_time <- function(x, secs) as.POSIXct(hms::round_hms(x, secs))
sent_scores <- function(x) syuzhet::get_sentiment(plain_tweets(x)) - .5

tt_sent <- gfg_data %>% 
  dplyr::mutate(days = round_time(created_at, 60*60*24), 
                sentiment = sent_scores(text)) %>% 
  dplyr::group_by(days) %>%
  dplyr::summarise(sentiment = sum(sentiment, na.rm = TRUE)) %>% 
  ggplot(aes(x = days, y = sentiment)) + 
  geom_point(aes(color = sentiment > 0)) + 
  geom_smooth(method = "loess", span = 0.2, color = "grey") + 
  scale_color_manual(values = c("#dd3333", "#22aa33")) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "#000000cc") + 
  theme_ipsum_rc() + theme(legend.position = "none")


#### SENTIMENT ANALYSIS PART 2 ####

# x <- readRDS("data/tt_tft.rds")
# my.data <- data.frame(colnames(x)[1:18])
# paste(my.data[,1], collapse = ", ")

dfttext1 <- gfg_data %>% 
  select(status_id, created_at, user_id, screen_name, text, source, 
         reply_to_status_id, reply_to_user_id, reply_to_screen_name, 
         is_quote, is_retweet, favorite_count, retweet_count, hashtags, 
         symbols, media_url, media_t.co, media_expanded_url, media_type,
         ext_media_url, ext_media_t.co, ext_media_expanded_url, 
         ext_media_type, mentions_user_id, mentions_screen_name, lang,
         quoted_status_id, quoted_text, retweet_status_id, retweet_text, 
         place_url, place_type, place_name, country, country_code, 
         geo_coords, coords_coords, bbox_coords) %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  dplyr::mutate(hours = round_time(created_at, 60 * 60)) %>%
  dplyr::group_by(hours)

dfttext2 <- dfttext1 %>%
  textfeatures(verbose = FALSE) %>% 
  dplyr::select(n_chars, n_commas, n_digits, n_exclaims, n_extraspaces, 
                n_hashtags, n_lowers, n_lowersp, n_mentions, n_periods, 
                n_urls, n_words, n_caps, n_nonasciis, n_puncts, 
                n_capsp, n_charsperword)

tweets.text <- data.frame("hrs" = dfttext1$hours, dfttext2) %>% 
  dplyr::group_by(hrs) 


ttplot <- tweets.text %>% gather(feature, n, -hrs) %>%
  ggplot(aes(x = hrs, y = n, colour = feature))  +
  geom_smooth(method = "loess", span = 0.5) +
  facet_wrap(~feature, ncol = 6) + 
  theme_ipsum_rc(base_size = 6, axis_text_size = 6, axis_title_size = 6) +
  theme(axis.text = element_text(size = rel(.68)), 
        strip.text = element_text(size = rel(.75)), 
        strip.text.x = element_text(margin = margin(5, 3, 6, 3, "pt")), panel.grid.major = element_line(colour = "black", size = rel(.08)), panel.grid.minor = element_line(colour = "black", size = rel(.05)), legend.position = "none") +
  labs(x = NULL, y = "Standardized Mean Frequencies")



#https://adb-8250698386551697.17.azuredatabricks.net




#### TWEETS CLEAN (for databricks) ####

tweets.clean <- gfg_data %>% 
  select(status_id, created_at, user_id, screen_name, text, 
         source, reply_to_status_id, reply_to_user_id, 
         reply_to_screen_name, is_quote, is_retweet, 
         favorite_count, retweet_count, quote_count, 
         reply_count, hashtags, symbols, urls_expanded_url, 
         media_expanded_url, media_type, mentions_user_id, 
         mentions_screen_name, lang, quoted_status_id, 
         quoted_text, quoted_created_at, 
         quoted_source, quoted_favorite_count, quoted_retweet_count, 
         quoted_user_id, quoted_screen_name, quoted_name, 
         quoted_followers_count, quoted_friends_count, 
         quoted_statuses_count, quoted_location, quoted_description, 
         quoted_verified, retweet_status_id, retweet_text, 
         retweet_created_at, retweet_source, retweet_favorite_count, 
         retweet_retweet_count, retweet_user_id, retweet_screen_name, 
         retweet_name, retweet_followers_count, retweet_friends_count, 
         retweet_statuses_count, retweet_location, retweet_description, 
         retweet_verified, place_name, country_code, geo_coords, 
         coords_coords, name, location, description,
         followers_count, friends_count, 
         favourites_count, account_created_at, verified)

# write.csv(tweets.clean, row.names = FALSE, file = "dfclean.csv")


#######################################
############# DATA TABLES #############

userDF <- tweets.clean %>% 
  select(user_id, screen_name, name, description, followers_count, 
         friends_count, favourites_count, account_created_at, verified) %>% 
  dplyr::distinct(user_id, .keep_all = TRUE)

nDF <- nrow(userDF)
userDF[1] <- cell_spec(userDF[[1]], background = "transparent", bold = T, 
                        color = spec_color(1:nDF, begin = 0.1,
                                           end = 0.25, 
                                           option = "turbo", 
                                           direction = -1),
                        extra_css=c("font-weight:300; text-shadow: 0.15px 0.15px 0.15px hsl(233deg 5% 26% / 85%), 0.25px 0.25px 0.25px hsl(233deg 5% 26% / 85%), 0 -0.75px 0px rgb(12 13 18 / 8%), 0 1px 0 rgb(255 255 255);"))

userDF[2] <- cell_spec(userDF[[2]], color = "white", bold = T, 
                       background = spec_color(1:nDF, begin = 0.1,
                                               end = 0.25, 
                                               option = "turbo", 
                                               direction = -1),
                       extra_css = c("box-shadow: 0.875px 0.875px 1.8px 0 rgb(0 0 0 / 30%), -0.75px -0.75px 1.5px 0 hsl(216deg 16% 94%), inset -0.5px -0.5px 1px 0 rgb(0 0 0 / 30%), inset 0.5px 0.5px 1px 0 hsl(220deg 23% 95%);"))



#######################################


tweetDF <- tweets.clean %>% 
  select(status_id, created_at, screen_name, text, source, 
         favorite_count, retweet_count, quote_count, reply_count, 
         hashtags, symbols, media_type, is_quote, is_retweet) %>% 
  dplyr::distinct(status_id, .keep_all = TRUE) %>% 
  arrange(desc(created_at)) %>%
  head(4)

ntweet <- nrow(tweetDF)
tweetDF[2] <- cell_spec(tweetDF[[2]], background = "transparent", bold = T, 
                        color = spec_color(1:ntweet, begin = 0.1,
                                           end = 0.25, 
                                                option = "turbo", 
                                                direction = -1),
                        extra_css=c("padding-left: 0px; font-weight: 300; text-shadow: 0.15px 0.15px 0.15px hsl(233deg 5% 26% / 85%), 0.25px 0.25px 0.25px hsl(233deg 5% 26% / 85%), 0 -0.75px 0px rgb(12 13 18 / 8%), 0 1px 0 rgb(255 255 255);"))
tweetDF[3] <- cell_spec(tweetDF[[3]], color = "white", bold = T, 
                        background = spec_color(1:ntweet, begin = 0.1,
                                                end = 0.25, 
                                           option = "turbo", 
                                           direction = -1),
                        extra_css = c("box-shadow: 0.875px 0.875px 1.8px 0 rgb(0 0 0 / 30%), -0.75px -0.75px 1.5px 0 hsl(216deg 16% 94%), inset -0.5px -0.5px 1px 0 rgb(0 0 0 / 30%), inset 0.5px 0.5px 1px 0 hsl(220deg 23% 95%);"))

# c(status_id, created_at, screen_name, text, source, 
#   favorite_count, retweet_count, quote_count, reply_count, 
#   hashtags, symbols, media_expanded_url, media_type, lang, 
#   location, is_quote, is_retweet,
#   place_name, country_code, geo_coords, coords_coords)

#######################################

replyDF <- tweets.clean %>% 
  dplyr::filter(reply_to_status_id != 'NA') %>% 
  select(status_id, created_at, reply_to_status_id, reply_to_user_id, 
         reply_to_screen_name, mentions_user_id, mentions_screen_name) %>% 
  #dplyr::distinct(reply_to_status_id, .keep_all = TRUE) %>%
  head()

nreply <- nrow(replyDF)
replyDF[3] <- cell_spec(replyDF[[3]], color = "white", bold = T, 
                         background = spec_color(1:nreply, end = 0.9, 
                                                 option = "A", 
                                                 direction = -1))


#######################################

retweetsDF <- tweets.clean %>% dplyr::filter(is_retweet == TRUE) %>%
  select(created_at, retweet_status_id, retweet_text, 
         retweet_created_at, retweet_source, retweet_favorite_count, 
         retweet_retweet_count, retweet_user_id, retweet_screen_name, 
         retweet_name, retweet_followers_count, retweet_friends_count, 
         retweet_statuses_count, retweet_location, retweet_description, 
         retweet_verified) %>% 
  dplyr::distinct(retweet_status_id, .keep_all = TRUE) %>%
  head()

nrt <- nrow(retweetsDF)
retweetsDF[1] <- cell_spec(retweetsDF[[1]], color = "white", bold = T, 
                           background = spec_color(1:nrt, end = 0.9, 
                                                   option = "A", 
                                                   direction = -1),
                           extra_css = c("box-shadow: 0.45px 1.5px 1.5px 0 rgb(0 0 0 / 12%), 0 1px 1px 0px rgb(0 0 0 / 24%), 0.75px 0.75px 0.5px 0 hsl(0deg 0% 100% / 50%) inset;"))

# c(created_at, is_quote, quoted_status_id, quoted_text, 
#   quoted_created_at, quoted_source, quoted_favorite_count, 
#   quoted_retweet_count, quoted_user_id, quoted_screen_name, 
#   quoted_name, quoted_followers_count, quoted_friends_count, 
#   quoted_statuses_count, quoted_location, quoted_description, 
#   quoted_verified)




dfvar <- data.frame(
  "n1" = c(01:13),
  "var" = c('status_id', 'created_at', 'user_id', 'screen_name', 
            'text', 'source', 'reply_to_screen_name', 'is_quote', 
            'is_retweet', 'favorite_count', 'retweet_count', 
            'quote_count', 'reply_count'),
  "n2" = c(14:26),
  "var2" = c('hashtags', 'symbols', 'media_expanded_url', 'media_type',  'mentions_screen_name', 'quoted_status_id', 'quoted_text', 'quoted_created_at', 'quoted_source', 'quoted_favorite_count', 'quoted_retweet_count', 'quoted_user_id', 'quoted_screen_name'),
  "n3" = c(27:39),
  "var3" = c('quoted_followers_count', 'quoted_location', 'quoted_description', 'quoted_verified', 'retweet_status_id', 'retweet_text', 'retweet_created_at', 'retweet_source', 'retweet_favorite_count', 'retweet_retweet_count', 'retweet_user_id', 'retweet_screen_name', 'retweet_followers_count'),
  "n4" = c(40:49, '', '', ''),
  "var4" = c('retweet_location', 'retweet_description', 'retweet_verified', 'name','location', 'description', 'followers_count', 'friends_count', 'account_created_at', 'verified', ' ', ' ', ' '))


nvarr <- nrow(dfvar)

dfvar[1] <- cell_spec(dfvar[[1]], background = "transparent", bold = T,
                      color = spec_color(1:nvarr,begin = 0, 
                                              end = 0.1,
                                              option = "turbo", 
                                              direction = 1))
dfvar[3] <- cell_spec(dfvar[[3]], background = "transparent", bold = T,
                      color = spec_color(1:nvarr,begin = 0.1, 
                                              end = 0.15,
                                              option = "turbo", 
                                              direction = 1))

dfvar[5] <- cell_spec(dfvar[[5]], background = "transparent", bold = T,
                      color = spec_color(1:nvarr, begin = 0.15,
                                              end = 0.2,
                                              option = "turbo", 
                                              direction = 1))
dfvar[1:10,7] <- cell_spec(dfvar[1:10,7], background = "transparent", bold = T,
                      color = spec_color(1:10, begin = 0.2,
                                              end = 0.25,
                                              option = "turbo", 
                                              direction = 1))

