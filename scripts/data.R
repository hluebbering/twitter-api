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

#### DEFINE COLORS & LAYOUTS
mypal11 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 40)
mypal10 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 70)
mypal7 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 7)
mypal8 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 15)


##################
#### READ CSV DATA

gfg_data <- 
  list.files(path = "data",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
 
gfg_data <- gfg_data %>%
  arrange(as.Date(gfg_data$created_at))

# write.csv(gfg_data, file = "musktweets1022.csv")



#### TWEETS DATAFRAME ####

#### RETWEETS ####


dfRetweet <- gfg_data %>% 
  dplyr::filter(is_retweet == TRUE) %>%
  dplyr::filter(retweet_screen_name != 'elonmusk') %>%
  dplyr::select(retweet_created_at, retweet_text, retweet_name, retweet_user_id, retweet_screen_name, retweet_favorite_count, retweet_retweet_count, retweet_location) %>%
  dplyr::arrange(desc(retweet_created_at))

topRetweet <-  dfRetweet %>% 
  dplyr::count(retweet_screen_name, sort = TRUE) %>% 
  head(12) %>% 
  dplyr::mutate(retweet_screen_name = reorder(retweet_screen_name, n)) %>%
  data.frame()

plotRetweet <- topRetweet %>% 
  ggplot(aes(y = retweet_screen_name, x = n, fill = as.factor(n), color = as.factor(n), label = n)) + 
  geom_bar(stat = "identity", color = "black", cex = 0.25) + 
  geom_text(hjust = -0.135, fontface="bold", size=4.5, family="Roboto Condensed") + 
  xlim(c(0, 565)) + 
  scale_fill_manual(values = mypal8) +
  scale_color_manual(values = mypal8) +
  xlab(NULL) + ylab(NULL) +
  coord_trans(expand = FALSE, xlim = c(-5, 605), ylim=c(0.25, 13)) +
  theme_ipsum_rc(base_size = 9, plot_margin = margin(r = 5, l = -10, t = 0, b = -90), plot_title_size = 10) + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 11.5, colour = "gray30", 
                                   margin = ggplot2::margin(t=0,b=0,l=0,r=5)),
        axis.text.x = element_text(size = 11, colour = "gray30"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray70", linetype = 3, size = 0.45),
        panel.grid.minor.x = element_line(color = "gray70", linetype = 3, size = 0.45),
        axis.ticks.y = element_line(colour = "gray20", lineend = 2))

tbody.style = tbody_style(color = "gray20", size = 12, fill = c("#f9f9f9", "white"), hjust=0.1, x=0.13)
colnames.style = colnames_style(color = "#333333", fill = "white", size = 11, face = "bold", hjust=0.1, x=0.13)

tableRetweet <- ggtexttable(
  head(topRetweet, 12), rows = NULL,
  theme = ttheme(colnames.style = colnames.style,
                 tbody.style = tbody.style)) %>%
  tab_add_hline(at.row = 1, row.side = "bottom", linewidth = 3.5, linecolor = "#dddddd") %>%
  tab_add_hline(at.row = 2:12, row.side = "bottom", linewidth = 2, linecolor = "#dddddd") %>%
  tab_add_hline(at.row = c(13), row.side = "bottom", linewidth = 3.5, linecolor = "#dddddd", linetype = 1) %>%
  tab_add_vline(at.column = 2, column.side = "left", from.row = 2, linetype = 1, linecolor = "#dddddd", linewidth = 0.75)



#### QUOTES ####

df_quote <- gfg_data %>%
  filter(is_quote==TRUE) %>% 
  select(c(quoted_status_id, quote_count, quoted_text, quoted_created_at, 
           quoted_source, quoted_favorite_count, quoted_retweet_count, 
           quoted_user_id, quoted_screen_name, quoted_name, 
           quoted_followers_count, quoted_friends_count, quoted_statuses_count, 
           quoted_location, quoted_description, quoted_verified))


#####################################
#####################################
############ TWEET WORDS ############

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

tweet_words <- gfg_data %>%
  dplyr::mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
                text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
                text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  dplyr::filter(!word %in% stop_words$word,
                !word %in% str_remove_all(stop_words$word, "'"),
                str_detect(word, "[a-z]"),
                !str_detect(word, "^#"),
                !str_detect(word, "^<"),
                !str_detect(word, "@\\S+"))

df.words <- tweet_words %>%
  dplyr::count(word, sort = TRUE) 


##### TOP 15 WORDS

dfSentWords <- data.frame(df.words) %>% head(15)
plotWords <- dfSentWords %>%
  ggplot(aes(y = reorder(word, -n), x = n, fill = as.factor(n))) +
  geom_histogram(stat = "identity", color = "black", cex = 0.25) +
  xlab(NULL) + ylab(NULL) +
  scale_fill_manual(values = mypal8) +
  theme_ipsum_rc(base_size = 9, plot_margin = margin(20, 8, 20, 8), 
                 plot_title_size = 10) + 
  theme(legend.position = "none")

table.words <- ggtexttable(dfSentWords, rows = NULL, 
                           theme = ttheme("mBlue", base_size = 8))


##### TOP 40 WORDS

top40 <- data.frame(df.words) %>% head(40)
packing <- circleProgressiveLayout(top40$n, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(top40, packing)
dat.gg <- circleLayoutVertices(packing, npoints=40)

plot.circle <- ggplot() + geom_polygon(data = dat.gg, aes(x, y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = mypal11) +
  geom_text(data = data, aes(x, y, size=n, label = word), color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin = ggplot2::margin(0,0,0,0)) + 
  coord_equal()


plot.lolli <- data.frame(df.words) %>% dplyr::filter(word != "flight") %>% 
  dplyr::filter(word != "tesla") %>% dplyr::filter(word != "lot") %>% 
  dplyr::filter(word != "hard") %>% dplyr::filter(word != "test") %>% 
  dplyr::filter(word != "cars") %>% head(38) %>% 
  ggplot(mapping = aes(x = reorder(word, -n), y = n, 
                       fill = reorder(word, -n), color = reorder(word, -n))) +
  geom_linerange(mapping = aes(x = word, ymin = 0, ymax = n), 
                 color = "lightgray", size = 1.25) +
  geom_point(size = 3.75, pch = 21) +
  geom_text(mapping = aes(label = n), color = "white", size = 1.5) +
  scale_color_manual(values = mypal10) + scale_fill_manual(values = mypal10) +
  xlab(NULL) + ylab(NULL) + theme_ipsum_rc() +
  theme_pubclean(base_size = 8, base_family = "Roboto Condensed", flip = FALSE) +
  coord_flip(expand = FALSE, clip = "on", ylim = c(-1, 750), xlim = c(0,39)) +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        plot.margin = ggplot2::margin(r = 0, l = 5, t = -1, b= -1), 
        axis.text.x = element_blank()) + ylim(c(-1, 700))

plot.lolli2 <- data.frame(df.words) %>% dplyr::filter(word != "flight") %>%
  dplyr::filter(word != "tesla") %>% dplyr::filter(word != "lot") %>%
  dplyr::filter(word != "hard") %>% dplyr::filter(word != "test") %>%
  dplyr::filter(word != "cars") %>% #dplyr::filter(word != "production") %>%
  dplyr::filter(word != "pretty") %>% dplyr::filter(word != "real") %>%
  dplyr::filter(word != "engine") %>%  
  head(39) %>%
  ggplot(mapping = aes(x = reorder(word, -n), y = n, fill = reorder(word, -n), color = reorder(word, -n))) +
  geom_linerange(mapping = aes(x = word, ymin = 0, ymax = n),
                 color = "lightgray", size = 1.45) +
  geom_point(size = 5.5, pch = 21, alpha=0.9, stroke =0.5, alpha = 0.85) +
  geom_text(mapping = aes(label = n), color = "white", size = 2.3, fontface="bold") +
  scale_color_manual(values = mypal10) + scale_fill_manual(values = mypal10) +
  xlab(NULL) + ylab(NULL) + theme_ipsum_rc() +
  theme_pubclean(base_size = 8, base_family = "Roboto Condensed", flip = FALSE) +
  coord_trans(expand = FALSE, clip = "on", ylim = c(30, 720), xlim = c(0,40)) +
  theme(legend.position = "none", axis.ticks.x = element_line(color = "black"),
        plot.margin = ggplot2::margin(r = 0, l = 0, t = -2.5, b= -1),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.75, size = 8.5, 
                                   margin = ggplot2::margin(t = 0.5, b = 0, r = 0, l = 0)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 6, margin = ggplot2::margin(t = 0, b = 0, r = -3, l = 0)),
        panel.grid.major.y = element_line(color = "gray70", linetype = 3, size = 0.3), 
        panel.grid.minor.y = element_line(color = "gray70", linetype = 3, size = 0.3), 
        axis.line.x  = element_line(color = "gray20", size = 0.45))


#########################################
#########################################
###### DATETIME (YEAR/MONTH/ETC) ########

dfTime <- gfg_data %>% dplyr::select(status_id, created_at) %>%
  dplyr::filter(year(created_at) > "2016")

dfTime$year <- year(dfTime$created_at)
dfTime$month <- months(dfTime$created_at) %>% factor(levels = month.name)
dfTime$weekday <- weekdays(dfTime$created_at) %>% 
  factor(levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

time <- as.POSIXct(strptime(hms::hms(hours = hour(dfTime$created_at),
  minutes = minute(dfTime$created_at)), "%H:%M"), "UTC")
x <- as.POSIXct(strptime(c("050000", "105959", "110000", "155959", "160000",  
                          "185959"), "%H%M%S"), "UTC")
dfTime$time <- case_when(
  between(time, x[1], x[2]) ~ "morning",
  between(time, x[3], x[4]) ~ "afternoon",
  between(time, x[5], x[6]) ~ "evening",
  TRUE ~ "night") %>% 
  factor(levels = c("morning", "afternoon", "evening", "night"))


#########################################
#### TWEET TYPES (RETWEET/QUOTE/ETC) ####

nGeneral <- nrow(gfg_data[gfg_data$is_retweet == FALSE, ] %>%
                   subset(is.na(reply_to_status_id)))
nMentions <- nrow(subset(gfg_data, !is.na(gfg_data$mentions_user_id)))
nReplies <- nrow(subset(gfg_data, !is.na(gfg_data$reply_to_status_id)))
nRetweets <- nrow(nRetweets <- gfg_data[gfg_data$is_retweet == TRUE, ])
nQuotes <- nrow(gfg_data[gfg_data$is_quote == TRUE, ])

tweetTypes <- data.frame(
  type = c("General", "Mentions", "Replies", "Retweets", "Quotes"),
  n = c(nGeneral, nMentions, nReplies, nRetweets, nQuotes)) %>%
  dplyr::arrange(dplyr::desc(n))


plot.types <- ggplot(tweetTypes) + 
  geom_bar(mapping = aes(x = type, y = n), stat = "identity", 
           width = 0.75, fill = "cornflowerblue", color = "navy") + 
  xlab(NULL) + ylab(NULL) +
  theme_ipsum_rc(base_size = 9, plot_margin = margin(20, 8, 20, 8), plot_title_size = 10) + 
  theme(legend.position = "none", axis.text.x = element_text(size = 10, face = "bold"))

table.types <- data.frame(tweetTypes) %>%
  ggtexttable(rows = NULL, theme = ttheme("mBlue", base_size = 12))


lolli.types <- tweetTypes %>%
  ggplot(mapping = aes(x = type, y = n, fill = reorder(type, -n), color = reorder(type, -n))) +
  geom_linerange(mapping = aes(x = type, ymin = -50, ymax = (n + 700)),
                 color = "gray80", size = 2, alpha=0.75) +
  geom_point(mapping= aes(size = as.factor(n), y = (n+1450)), pch = 21, alpha = 0.95, stroke=1.25) +
  scale_size_manual(values = c(8.75, 9.75, 10.25, 10.7, 10.7)) +
  geom_text(mapping = aes(label = n, y = (n+1450)), color = "white", size = 3.25, fontface = "bold") +
  scale_color_manual(values = mypal7) + 
  scale_fill_manual(values = mypal7) +
  xlab(NULL) + ylab(NULL) + 
  coord_trans(expand = FALSE, ylim = c(0, 16300), xlim=c(0.5, 5.5)) +
  theme_ipsum_rc(base_size = 9, plot_margin = ggplot2::margin(r = 5, l = 5, t = 10, b = 0), plot_title_size = 10) + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 13, face="bold", colour = "gray20",
                                   margin = ggplot2::margin(t=6,b=0,l=0,r=0)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray70", linetype = 3, size = 0.45),
        panel.grid.minor.y = element_line(color = "gray70", linetype = 3, size = 0.45),
        axis.line.x  = element_line(color = "gray20", size = 0.45),
        axis.ticks.x = element_line(colour = "gray20", lineend = 2))




##############################
######## TOP MENTIONS ########

dfMentions <- gfg_data$mentions_screen_name %>%
  strsplit(" ") %>% unlist(use.names = FALSE) %>% tolower %>%
  table() %>% sort(decreasing = TRUE) %>%
  data.frame() %>% dplyr::rename(c("screen_name" = ".")) %>% 
  dplyr::rename(c("n" = "Freq"))

plot.Mentions <- head(dfMentions, 12) %>%
  ggplot(mapping = aes(y = screen_name, x = n, fill = as.factor(n), color = as.factor(n))) +
  geom_bar(stat = "identity", color = "black", cex = 0.25)  +
  scale_fill_manual(values = mypal8) +
  scale_color_manual(values = mypal8) +
  xlab(NULL) + ylab(NULL) +
  coord_trans(expand = FALSE, xlim = c(-5, 1495), ylim=c(0.25, 13)) +
  theme_ipsum_rc(base_size = 9, plot_margin = margin(r = 5, l = 5, t = 0, b = -60), plot_title_size = 10) + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 11.5, colour = "gray30", 
                                   margin = ggplot2::margin(t=0,b=0,l=0,r=5)),
        axis.text.x = element_text(size = 11, colour = "gray30"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray70", linetype = 3, size = 0.45),
        panel.grid.minor.x = element_line(color = "gray70", linetype = 3, size = 0.45),
        axis.ticks.y = element_line(colour = "gray20", lineend = 2))

plot.Mentions2 <- plot.Mentions + 
  geom_text(mapping = aes(label=n), hjust = -0.08, fontface="bold", size=4.45, family="Roboto Condensed")
  


tbody.style = tbody_style(color = "gray20", size = 12, fill = c("#f9f9f9", "white"), hjust=0.1, x=0.13)
colnames.style = colnames_style(color = "#333333", fill = "white", size = 13, face = "bold", hjust=0.1, x=0.13)

table.Mentions <- ggtexttable(
  head(dfMentions, 12), rows = NULL,
  theme = ttheme(colnames.style = colnames.style,
                 tbody.style = tbody.style)) %>%
  tab_add_hline(at.row = 1, row.side = "bottom", linewidth = 3.5, linecolor = "#dddddd") %>%
  tab_add_hline(at.row = 2:12, row.side = "bottom", linewidth = 2, linecolor = "#dddddd") %>%
  tab_add_hline(at.row = c(13), row.side = "bottom", linewidth = 3.5, linecolor = "#dddddd", linetype = 1) %>%
  tab_add_vline(at.column = 2, column.side = "left", from.row = 2, linetype = 1, linecolor = "#dddddd", linewidth = 0.75)



##############################
######## TOP HASHTAGS ########

dfTags <- gfg_data$hashtags %>%
  strsplit(" ") %>% unlist(use.names = FALSE) %>% tolower %>%
  table() %>% sort(decreasing = TRUE) %>%
  data.frame() %>% dplyr::rename(c("Hashtag" = "."))


plotTags <- head(dfTags, 12) %>%
  ggplot(aes(y = Hashtag, x = Freq, fill = Freq)) +
  geom_bar(stat = "identity", color = "black", cex = 0.25)  +
  xlab(NULL) + ylab(NULL) +
  theme_ipsum_rc(base_size = 9, plot_margin = margin(20,8,20,8), plot_title_size = 10) + 
  theme(legend.position = "none")

tableTags <- ggtexttable(head(dfTags, 12), rows = NULL, 
                         theme = ttheme("mBlue", base_size = 8))

###################################
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

dfttext1 <- gfg_data %>% 
  dplyr::select(status_id, created_at, user_id, screen_name, text) %>%
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
        strip.text.x = element_text(margin = margin(5, 3, 6, 3, "pt")), 
        panel.grid.major = element_line(colour = "black", size = rel(.08)), 
        panel.grid.minor = element_line(colour = "black", size = rel(.05)), 
        legend.position = "none") +
  labs(x = NULL, y = "Standardized Mean Frequencies")


# x <- readRDS("data/tt_tft.rds")
# my.data <- data.frame(colnames(x)[1:18])
# paste(my.data[,1], collapse = ", ")


#######################################
#######################################
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
         followers_count, friends_count, statuses_count,
         favourites_count, account_created_at, verified)

# write.csv(tweets.clean, row.names = FALSE, file = "dfclean.csv")


##################################
############# TABLES #############

##################
###### USER ######

userDF <- tweets.clean %>% 
  select(user_id, screen_name, name, description, followers_count, statuses_count, 
         friends_count, favourites_count, account_created_at, verified) %>% 
  dplyr::distinct(user_id, .keep_all = TRUE)

userDF[1] <- cell_spec(userDF[[1]],
  background = "transparent", bold = T,
  color = spec_color(1:nrow(userDF),
    begin = 0.15, end = 0.25, option = "turbo", direction = -1),
  extra_css = c("font-size: 11px; font-weight:400; text-shadow: 0.15px 0.15px 0.15px hsl(233deg 5% 26% / 85%), 0.25px 0.25px 0.25px hsl(233deg 5% 26% / 85%), 0 -0.75px 0px rgb(12 13 18 / 8%), 0 1px 0 rgb(255 255 255);"))

userDF[2] <- cell_spec(userDF[[2]],
  color = "white", bold = T,
  background = spec_color(1:nrow(userDF),
    begin = 0.15, end = 0.25, option = "turbo", direction = -1),
  extra_css = c("font-size: 12px; padding: 1pt 4pt; box-shadow: 0.875px 0.875px 1.8px 0 rgb(0 0 0 / 30%), -0.75px -0.75px 1.5px 0 hsl(216deg 16% 94%), inset -0.5px -0.5px 1px 0 rgb(0 0 0 / 30%), inset 0.5px 0.5px 1px 0 hsl(220deg 23% 95%);"))


####################
###### TWEETS ######

tweetDF <- tweets.clean %>%
  select(
    status_id, created_at, screen_name, text, source,
    favorite_count, retweet_count, quote_count, reply_count,
    hashtags, symbols, media_type, is_quote, is_retweet) %>%
  dplyr::distinct(status_id, .keep_all = TRUE) %>%
  arrange(desc(created_at)) %>% head(3)

tweetDF[2] <- cell_spec(tweetDF[[2]],
  background = "transparent", bold = T,
  color = spec_color(1:nrow(tweetDF),
    begin = 0.15, end = 0.25, option = "turbo", direction = -1),
  extra_css = c("font-size: 11.5px; padding-left: 0px; font-weight: 300; text-shadow: 0.15px 0.15px 0.15px hsl(233deg 5% 26% / 85%), 0.25px 0.25px 0.25px hsl(233deg 5% 26% / 85%), 0 -0.75px 0px rgb(12 13 18 / 8%), 0 1px 0 rgb(255 255 255);"))

tweetDF[3] <- cell_spec(tweetDF[[3]],
  color = "white", bold = T,
  background = spec_color(1:nrow(tweetDF),
    begin = 0.15, end = 0.25, option = "turbo", direction = -1),
  extra_css = c("font-size: 11px; padding: 1pt 4pt; box-shadow: 0.875px 0.875px 1.8px 0 rgb(0 0 0 / 30%), -0.75px -0.75px 1.5px 0 hsl(216deg 16% 94%), inset -0.5px -0.5px 1px 0 rgb(0 0 0 / 30%), inset 0.5px 0.5px 1px 0 hsl(220deg 23% 95%);"))


#####################
###### REPLIES ######


dfReplies <- gfg_data$reply_to_screen_name %>%
  strsplit(" ") %>% unlist(use.names = FALSE) %>% tolower %>%
  table() %>% sort(decreasing = TRUE) %>%
  data.frame() %>% dplyr::rename(c("screen_name" = ".")) %>% 
  dplyr::rename(c("n" = "Freq")) %>% 
  dplyr::filter(screen_name != 'elonmusk')


plotReplies <- dfReplies %>% head(12) %>%
  ggplot(aes(y = screen_name, x = n, fill = as.factor(n), color = as.factor(n), label = n)) + 
  geom_bar(stat = "identity", color = "black", cex = 0.25) + 
  geom_text(hjust = -0.135, fontface="bold", size=4.5, family="Roboto Condensed") + 
  xlim(c(0, 565)) + 
  scale_fill_manual(values = mypal8) +
  scale_color_manual(values = mypal8) +
  xlab(NULL) + ylab(NULL) +
  coord_trans(expand = FALSE, xlim = c(-5, 400), ylim=c(0.25, 13)) +
  theme_ipsum_rc(base_size = 9, plot_margin = margin(r = 5, l = 5, t = 0, b = -60), plot_title_size = 10) + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 11.5, colour = "gray30", 
                                   margin = ggplot2::margin(t=0,b=0,l=0,r=5)),
        axis.text.x = element_text(size = 11, colour = "gray30"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray70", linetype = 3, size = 0.45),
        panel.grid.minor.x = element_line(color = "gray70", linetype = 3, size = 0.45),
        axis.ticks.y = element_line(colour = "gray20", lineend = 2))
  


tbody.style = tbody_style(color = "gray20", size = 12, fill = c("#f9f9f9", "white"), hjust=0.1, x=0.13)
colnames.style = colnames_style(color = "#333333", fill = "white", size = 11, face = "bold", hjust=0.1, x=0.13)

tableReplies <- ggtexttable(
  head(dfReplies, 12), rows = NULL,
  theme = ttheme(colnames.style = colnames.style,
                 tbody.style = tbody.style)) %>%
  tab_add_hline(at.row = 1, row.side = "bottom", linewidth = 3.5, linecolor = "#dddddd") %>%
  tab_add_hline(at.row = 2:12, row.side = "bottom", linewidth = 2, linecolor = "#dddddd") %>%
  tab_add_hline(at.row = c(13), row.side = "bottom", linewidth = 3.5, linecolor = "#dddddd", linetype = 1) %>%
  tab_add_vline(at.column = 2, column.side = "left", from.row = 2, linetype = 1, linecolor = "#dddddd", linewidth = 0.75)



#####################
###### QUOTES ######

dfqt <- tweets.clean %>% 
  dplyr::select(quoted_screen_name) %>% 
  dplyr::filter(quoted_screen_name != 'NA') %>% 
  dplyr::filter(quoted_screen_name != 'elonmusk') %>% 
  dplyr::count(quoted_screen_name, sort = TRUE) %>% 
  dplyr::mutate(quoted_screen_name = reorder(quoted_screen_name, n))

plotQuotes <- dfqt %>% head(12) %>% 
  ggplot(aes(y = quoted_screen_name, x = n, fill = n)) + 
  geom_bar(stat = "identity", color = "black", cex = 0.25) + 
  xlab(NULL) + ylab(NULL) + labs(title = "Users Quoted") + 
  theme_ipsum_rc(base_size = 10, plot_margin = ggplot2::margin(10,10,10,10), plot_title_size =  12)  + 
  theme(legend.position = "none")


#######################################
########## DATA VARIABLES ##########

dfvar <- data.frame(
  "n1" = c(1:13),
  "var" = c('status_id', 'created_at', 'user_id', 'screen_name', 
            'text', 'source', 'reply_to_screen_name', 'is_quote', 
            'is_retweet', 'favorite_count', 'retweet_count', 
            'quote_count', 'reply_count'),
  "n2" = c(14:26),
  "var2" = c('hashtags', 'symbols', 'media_expanded_url', 'media_type',  
             'mentions_screen_name', 'quoted_status_id', 'quoted_text', 
             'quoted_created_at', 'quoted_source', 'quoted_favorite_count', 
             'quoted_retweet_count', 'quoted_user_id', 'quoted_screen_name'),
  "n3" = c(27:39),
  "var3" = c('quoted_followers_count', 'quoted_location', 'quoted_description', 
             'quoted_verified', 'retweet_status_id', 'retweet_text', 'retweet_created_at', 
             'retweet_source', 'retweet_favorite_count', 'retweet_retweet_count', 
             'retweet_user_id', 'retweet_screen_name', 'retweet_followers_count'),
  "n4" = c(40:50, '', ''),
  "var4" = c('retweet_location', 'retweet_description', 'retweet_verified', 
             'name','location', 'description', 'followers_count', 'friends_count', 
             'statuses_count', 'account_created_at', 'verified', ' ', ' '))

dfvar[1] <- cell_spec(dfvar[[1]],
  background = "transparent", bold = T,
  color = spec_color(1:nrow(dfvar),
    begin = 0, end = 0.1, option = "turbo", direction = 1))

dfvar[3] <- cell_spec(dfvar[[3]], 
  background = "transparent", bold = T,
  color = spec_color(1:nrow(dfvar),
    begin = 0.1, end = 0.15, option = "turbo", direction = 1))

dfvar[5] <- cell_spec(dfvar[[5]], 
  background = "transparent", bold = T,
  color = spec_color(1:nrow(dfvar), 
    begin = 0.15, end = 0.2, option = "turbo", direction = 1))

dfvar[1:11,7] <- cell_spec(dfvar[1:11,7], 
  background = "transparent", bold = T,
  color = spec_color(1:11, 
    begin = 0.2, end = 0.25, option = "turbo", direction = 1))



