library(syuzhet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)

musk.search <- read.csv("dfclean.csv")
musk.search$created_at = as.POSIXct(musk.search$created_at)

musk.search <- musk.search[musk.search$created_at > "2017-12-30", ] %>%
  dplyr::select(status_id, created_at, screen_name, text,favorite_count,
                reply_to_screen_name, reply_to_status_id, reply_to_user_id, 
                is_retweet, retweet_count, retweet_text, quoted_text, 
                mentions_user_id, mentions_screen_name, hashtags)

musk.text  = as.vector(musk.search$text)
cleanTweet = gsub("rt|RT", "", musk.text)
cleanTweet = gsub("http\\w+", "", cleanTweet)
cleanTweet = gsub("<.*?>", "", cleanTweet)
cleanTweet = gsub("@\\w+", "", cleanTweet)
cleanTweet = gsub("[[:punct:]]", "", cleanTweet)
cleanTweet  = gsub("\r?\n|\r", " ", cleanTweet)
cleanTweet = gsub("[[:digit:]]", "", cleanTweet)
cleanTweet = gsub("[ |\t]{2,}", "", cleanTweet)
cleanTweet = gsub("^ ", "", cleanTweet)
cleanTweet = gsub(" $", "", cleanTweet)

# musk.text.sentiment <- get_nrc_sentiment(cleanTweet)
# musk.text.final <- cbind(musk.search, musk.text.sentiment)
# saveRDS(musk.text.final, file = "nrc_sentiment.rds")

nrc_sentiment <- read_rds("nrc_sentiment.rds") %>% 
  dplyr::select(created_at, anger, anticipation,
                disgust, fear, joy, sadness, surprise, trust, 
                negative, positive)


#### SENTIMENT ANALYSIS PART 1 ####

library(paletteer)
mypal1 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 9)
mypal2 <- get_palette(c(paletteer_d("lisa::CyTwombly")), 8)
mypal3 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 15)
mypal4 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 3)
mypal5 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 5)
mypal6 <- get_palette(c("#001964FF", paletteer_d("fishualize::Coryphaena_hippurus")[1:2], paletteer_d("fishualize::Prionace_glauca")[3:5]), 8)


df.sent1 <- gather(nrc_sentiment, "sentiment", "values", 2:9) %>% 
  dplyr::group_by(sentiment) %>%
  dplyr::summarise("total" = sum(values))

plot.sent1 <- df.sent1 %>% 
  ggplot(aes(y = sentiment, x = total)) + 
  geom_bar(aes(fill = sentiment), stat = "identity", color="transparent") +  
  scale_fill_manual(values = mypal1) + 
  xlab("freq") + ylab("sentiment") + 
  theme_ipsum_rc(base_size = 10, plot_margin = margin(10,10,10,10)) + 
  theme(legend.position = "none")


pie_words <- df.sent1 %>% arrange(desc(total))
plot.sent1B <- ggplot(pie_words, aes(x = "", y = -total, fill = reorder(sentiment, -total))) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_polar("y") + theme_ipsum_rc() + 
  theme_void() + scale_fill_manual(values = mypal1) +
  theme(legend.position = "right", legend.title = element_blank())

#### SENTIMENT ANALYSIS PART 2 ####

df.sent2 <- gather(nrc_sentiment, "sentiment", "values", 2:9) %>% 
  dplyr::group_by(sentiment) %>% 
  dplyr::select(created_at, sentiment, values)
df.sent2 <- data.frame(df.sent2)

plot.sent2 <- ggplot(data = df.sent2) + 
  geom_smooth(mapping = aes(x = created_at, y = values, color = sentiment), method = "loess", span=0.1, size = 1) + 
  scale_color_manual(values = mypal6) + 
  ylab(NULL) + xlab(NULL) + 
  theme_ipsum_rc(base_size = 8, axis_title_size = 8, axis_text_size = 8, plot_margin = margin(5,5,5,5)) + 
  scale_y_continuous(expand=c(0, 0)) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  theme(strip.text = element_text(color = "black", size = 8, family = "Roboto Condensed"), legend.position = "none")



#### SENTIMENT ANALYSIS PART 2B ####


df.sent2A <- nrc_sentiment %>%
  gather("sentiment", "values", 2:9) %>% 
  dplyr::group_by(sentiment) %>% 
  dplyr::select(created_at, sentiment, values) %>%
  dplyr::arrange(desc(created_at)) %>% 
  dplyr::mutate(created_at = lubridate::floor_date(created_at, "month")) %>%
  dplyr::group_by(created_at, sentiment) %>% 
  summarise(n = sum(values))

plot.sent2A <- ggplot(data = df.sent2A) + 
  geom_bar(mapping = aes(x = created_at, y = n, fill = sentiment), stat = "identity") + 
  scale_fill_manual(values = mypal1) + 
  ylab(NULL) + xlab(NULL) + 
  theme_ipsum_rc(base_size = 8, axis_title_size = 8, axis_text_size = 8, plot_margin = margin(5,5,5,5)) + 
  facet_wrap(~sentiment, scales = "free_x") + 
  theme(strip.text = element_text(color = "black", size = 8, family = "Roboto Condensed", face = "bold"), legend.position = "none", )

plot.sent2B <- df.sent2A %>%
  ggplot(aes(x=created_at, y=n)) +
  geom_bar(mapping = aes(x = created_at, y = n, fill = sentiment),
           stat = "identity", alpha=0.25) +
  geom_area(aes(fill = sentiment), alpha=0.25) +
  geom_line(aes(color = sentiment)) +
  scale_fill_manual(values = mypal1) +
  scale_color_manual(values = mypal1) +
  ylab(NULL) + xlab(NULL) + 
  facet_wrap(~sentiment, scales = "fixed") +
  theme_ipsum_rc(base_size = 8, axis_title_size = 8, axis_text_size = 8, plot_margin = margin(10,10,10,10)) +
  theme(strip.text = element_text(color = "black", size = 8, family = "Roboto Condensed"),
        legend.position="none", panel.spacing = unit(0.25, "lines"), 
        #axis.ticks.x=element_blank(),
        axis.text.x = element_text(size = 8)) +
  ylim(c(0, 180))


#### SENTIMENT ANALYSIS PART 3 ####

df.sent3 <- gather(nrc_sentiment, "Polarity", "values", 10:11)  %>% 
  group_by(Polarity) %>%
  summarise(total = sum(values))

plot.sent3 <- ggplot(data = df.sent3, aes(x = Polarity, y = total)) +
  geom_bar(aes(fill = Polarity), stat = "identity") +
  scale_fill_manual(values = mypal2) +
  theme_ipsum_rc(base_size = 8, axis_text_size = 8, plot_margin = margin(20,20,20,20)) + 
  theme(legend.position = "none") +
  xlab(NULL) + ylab(NULL) + 
  geom_text(aes(label =   total), position = position_dodge(width=0.75), vjust = -0.25)



#### RIDGELINE PLOT ####


library(ggridges)
plot.ridges1 <- ggplot(df.sent2A, aes(x = n, y = fct_reorder(sentiment, n), fill = fct_reorder(sentiment, n))) +
  geom_density_ridges(alpha = .85, color = "black", scale = 2.5, rel_min_height = .0001, cex = 0.3) +
  guides(fill = FALSE) +
  theme_ridges(font_size = 8, font_family = "Roboto Condensed") +
  theme_ipsum_rc(plot_margin = ggplot2::margin(t=30, r=20, b=0, l=20)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values = mypal1) +
  #theme_minimal(base_size = 8, base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 12, color = "black", family = "Roboto Condensed"),
        plot.margin = ggplot2::margin(t=30, r=30, b=0, l=30)) +
  scale_y_discrete(expand = c(0.005, 0)) + 
  scale_x_continuous(expand = c(0.085, 0))

plot.ridges2 <- ggplot(df.sent2A, aes(x = n, y = factor(year(created_at), levels = c("2022", "2021", "2020", "2019", "2018")), fill = factor(year(created_at)))) +
  geom_density_ridges(alpha = .85, color = "black", scale = 2.5, rel_min_height = .001, cex = 0.3) +
  guides(fill = FALSE) +
  theme_ridges() +
  facet_wrap(~sentiment) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values = mypal3) +
  theme_minimal(base_size = 8, base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 8, color = "black")) +
  xlim(c(-10, 250)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0))


######################
#### RADAR PLOT ####

library(dplyr)
library(scales)
library(tibble)
library(tidytuesdayR)
library(tidyverse)
library(ggradar)

data2 <- nrc_sentiment %>%
  dplyr::select(created_at, 2:9) %>%
  dplyr::arrange(desc(created_at)) %>%
  dplyr::mutate(month = lubridate::floor_date(created_at, "year")) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    Anger = sum(anger), Anticipation = sum(anticipation),
    Disgust = sum(disgust), Fear = sum(fear), Joy = sum(joy),
    Sadness = sum(sadness), Surprise = sum(surprise), Trust = sum(trust)
  )
data2$month <- factor(as.Date(data2$month))
group <- t(names(data2))


plotRadar <- data2 %>% ggradar(
  grid.min = 100, grid.mid = 700, grid.max = 1300,
  values.radar = c("100", "800", "1300"),
  group.point.size = 2, group.line.width = 1,
  grid.label.size = 3, axis.label.size = 3,
  gridline.mid.colour = "grey", group.colours = "#56638A"
) +
  facet_wrap(vars(group)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 8, family = "Roboto", face = "bold"),
    text = element_text(family = "Roboto"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = ggplot2::margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8, margin = margin(0, 0, 10, 0))
  )




######################
#### RADAR PLOT 2 ####

library(viridisLite)
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(fmsb)
library(colormap)

radarDF2 <- nrc_sentiment %>%
  dplyr::mutate(group = year(created_at)) %>%
  dplyr::select(group, anger, anticipation, disgust, fear,
                joy, sadness, surprise, trust) %>%
  as_tibble(rownames = NULL) %>%
  dplyr::group_by(group) %>% 
  dplyr::summarise("anger" = sum(anger), 
                   "anticipation" = sum(anticipation),
                   "disgust" = sum(disgust),
                   "fear" = sum(fear),
                   "joy" = sum(joy),
                   "sadness" = sum(sadness),
                   "surprise" = sum(surprise),
                   "trust" = sum(trust)) %>% 
  rbind(rep(1200,8) , rep(50,8)) %>%
  dplyr::mutate_at(dplyr::vars(-group), scales::rescale) %>%
  dplyr::select(-group)

# Prepare title
mytitle <- c("2018", "2019", "2020", "2021", "2022")
colors_border = c("#001964", "#124F94", "#2894DA", "#01C4E5", "#A0F5F7")
colors_in = c("#0019644c", "#124F944c", "#2894DA4c", "#01C4E54c", "#A0F5F74c")
  #colormap(colormap = mypal5, nshades = 5, alpha = 0.3)


myradarChart <- function(radarDF2, colors_border, colors_in, mytitle) {
  par(mar = rep(0.8, 4))
  par(mfrow = c(2, 3))
  for (i in 1:5) {
    radarchart(radarDF2[c(6, 7, i), ],
               axistype = 1,
               
               # custom polygon
               pcol = colors_border[i], pfcol = colors_in[i],
               plwd = 4, plty = 1,
               
               # custom the grid
               cglcol = "grey", cglty = 1, axislabcol = "grey",
               caxislabels = seq(200, 1200, 200), cglwd = 0.6,
               
               # custom labels
               vlcex = 0.9,
               title = mytitle[i]
    )
  }
}


######################################################
######################################################
############## RETWEET USER TIMELINE #################


users2 <- c("SpaceX", "Tesla", "NASA", "cleantechnica", "ElectrekCo", "OpenAI", "InsideEVs", "Hyperloop", "boringcompany")
# top_users <- lookup_users(users2)
# tmls <- get_timelines(c("SpaceX", "Tesla"), n = 9200)
# saveRDS(tmls, file = "user_timeline.rds")

tmls <- readRDS("user_timeline.rds")
tmls2 <- tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  dplyr::select(created_at, screen_name) %>%
  dplyr::mutate(created_at = lubridate::floor_date(created_at, "month"))  %>%
  dplyr::group_by(created_at, screen_name) %>% 
  summarise(n = n())

plot.timeline <- ggplot(tmls2, mapping = aes(x=created_at, y=n, color = fct_reorder(screen_name, n)))+
  geom_line(size=1)+ geom_point(aes(fill = fct_reorder(screen_name, n)), size = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = n, fill = fct_reorder(screen_name, n)), alpha = 0.25) +
  #facet_wrap(~ screen_name, ncol = 1, scale = "free_x") +
  expand_limits(y = 0) + 
  scale_color_manual(values = mypal4) +
  scale_fill_manual(values = mypal4) +
  ggtitle("Frequency of Tweets per User Over Time") +
  ylab(NULL) + xlab(NULL) +
  theme_ipsum_rc(plot_title_size = 10) +
  theme(legend.position="bottom", legend.title = element_blank())


#### DONUT CHART ####

# top_users <- lookup_users(users2) 
# saveRDS(top_users, file = "user_lookup.rds")
top_users <- readRDS("user_lookup.rds")
timelineDF <- top_users %>% dplyr::select(user_id, screen_name, name, description, followers_count, friends_count, listed_count, statuses_count, account_created_at,  location, verified)

dataTL <- timelineDF %>% dplyr::select(name, screen_name, followers_count, listed_count, statuses_count) %>%
  dplyr::arrange(dplyr::desc(statuses_count))

dataTL$fraction <- dataTL$statuses_count / sum(dataTL$statuses_count)
dataTL$ymax <- cumsum(dataTL$fraction)
dataTL$ymin <- c(0, head(dataTL$ymax, n=-1))
dataTL$labelPosition <- (dataTL$ymax + dataTL$ymin) / 2
dataTL$label <- paste0(dataTL$screen_name, "\n ", dataTL$statuses_count)
dataTL$label[c(7:9)] <- c("", "", "")
dataTL$labelPosition[c(5:6)] <- c(0.94273681, 0.99)

plot.donuts <- ggplot(dataTL, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(name, statuses_count))) +
  geom_rect(color="black") +
  geom_text( x=5, aes(y=labelPosition, label=label, color=reorder(name, statuses_count)), size=3) +
  scale_fill_manual(values=rev(mypal1)) +
  scale_color_manual(values=rev(mypal1)) +
  coord_polar(theta="y") +
  xlim(c(.35, 4.5)) +
  theme_void() +
  theme(legend.position = "right", legend.title = element_blank(),
        plot.margin = margin(5,5,5,5), legend.margin = ggplot2::margin(0,0,0,0),
        legend.text = element_text(size = 7), legend.key.size = ggplot2::unit(0.2, "cm"))






