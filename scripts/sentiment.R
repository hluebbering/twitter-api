library(syuzhet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)

musk.search <- read.csv("musktweets1022.csv")
musk.search$created_at = as.POSIXct(musk.search$created_at)

musk.search <- musk.search[musk.search$created_at > "2021-12-01", ] %>%
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

musk.text.sentiment = get_nrc_sentiment(cleanTweet)
musk.text.final = cbind(musk.search, musk.text.sentiment)


#### SENTIMENT ANALYSIS PART 1 ####

df.sent1 <- gather(musk.text.final, "sentiment", "values", 16:23) %>% 
  dplyr::group_by(sentiment) %>%
  dplyr::summarise("total" = sum(values))

plot.sent1 <- df.sent1 %>% 
  ggplot(aes(x = sentiment, y = total)) +
  geom_bar(aes(fill = sentiment), stat = "identity", color="transparent") + 
  # scale_fill_manual(values = c("pink", "blue", "navy", "hotpink",
  #                     "green", "yellow", "orange", "purple")) +
  xlab("Emotions") + ylab("Total") + 
  theme_ipsum_rc(base_size = 10, plot_margin = margin(20, 20, 20, 20)) + 
  theme(legend.position = "none") + coord_flip()


#### SENTIMENT ANALYSIS PART 2 ####

df.sent2 <- gather(musk.text.final, "sentiment", "values", 16:23) %>% 
  dplyr::group_by(sentiment) %>% 
  dplyr::select(created_at, screen_name, text, negative, positive, sentiment, values)
df.sent2 <- data.frame(df.sent2)

plot.sent2 <- ggplot(data = df.sent2) +
  geom_smooth(mapping = aes(x = created_at, y = values, color = sentiment), 
              method = "loess", span=0.1, size = 1) + 
  scale_y_continuous(expand=c(0, 0)) +
  facet_wrap(~sentiment, scales = "free_y") + 
  theme_ipsum_rc(base_size = 8, axis_title_size = 8, 
                  plot_margin = margin(15,15,15,15)) +
  theme(strip.text = element_text(color = "white", size = 10, family = "Roboto"),
        legend.position = "none") + xlab(NULL)

#### SENTIMENT ANALYSIS PART 3 ####

df.sent3 <- gather(musk.text.final, "Polarity", "values", 24:25)  %>% 
  group_by(Polarity) %>%
  summarise(total = sum(values))

plot.sent3 <- ggplot(data = df.sent3, aes(x = Polarity, y = total)) +
  geom_bar(aes(fill = Polarity), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("total") + 
  ggtitle("Sentiment Analyis") +
  geom_text(aes(label =   total), position = position_dodge(width=0.75), vjust = -0.25)


#### SENTIMENT ANALYSIS PART 4 ####

df.sent5 <- select(musk.text.final, created_at, 16:23)
df.sent5 <- separate(df.sent5, created_at, c("date","Time")," ") %>%
  group_by(date) %>%
  summarise(Anger = sum(anger), Anticipation = sum(anticipation), 
            Disgust = sum(disgust), Fear = sum(fear), Joy = sum(joy), 
            Sadness = sum(sadness), Surprise = sum(surprise), Trust = sum(trust))

df.sent5$date <- as.Date(df.sent5$date,"%Y-%m-%d") 
df.sent5$date <- as.Date(cut(df.sent5$date, breaks = "month"))

df.sent5 <- gather(df.sent5, "sentiment", "values", 2:9) %>%
  group_by(date, sentiment)%>%
  summarise(Total=sum(values))

plot.sent5 <- ggplot(data = df.sent5, aes(x = date, y = Total, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment,stat = "identity")) +
  geom_point(size = 0.5) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Total") + 
  scale_y_continuous(limits=c(0,150))



######################

tidy_tweets <- musk.search %>% 
  filter(is_retweet==FALSE)%>% 
  select(status_id, text)

tidy.text  = as.vector(tidy_tweets$text)
cleanTweet = gsub("rt|RT", "", tidy.text)
cleanTweet = gsub("http\\w+", "", cleanTweet)
cleanTweet = gsub("<.*?>", "", cleanTweet)
cleanTweet = gsub("@\\w+", "", cleanTweet)
cleanTweet = gsub("[[:punct:]]", "", cleanTweet)
cleanTweet  = gsub("\r?\n|\r", " ", cleanTweet)
cleanTweet = gsub("[[:digit:]]", "", cleanTweet)
cleanTweet = gsub("[ |\t]{2,}", "", cleanTweet)
cleanTweet = gsub("^ ", "", cleanTweet)
cleanTweet = gsub(" $", "", cleanTweet)

nrc <- get_nrc_sentiment(cleanTweet)
nrc_words <- cbind(tidy_tweets, nrc)

pie_words <- gather(nrc_words, "sentiment", "values", 3:12) %>% 
  dplyr::group_by(sentiment) %>%
  filter(values>0) %>%
  tally() %>% 
  arrange(desc(n))


