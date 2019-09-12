
library(rtweet)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(scico)

#llr <- search_users(q = "larsloekke", n = 1)

#tweets_data(llr)

# df <- search_tweets(
#   "to:larsloekke",
#   n = 1000,
#   retryonratelimit = TRUE)
# 
# df <- df %>%
#   filter(reply_to_status_id=="1167729739114500097")
# 
# df[sapply(df, is.list)] <- lapply(df[sapply(df, is.list)],
#                                     as.character)
# write_csv(df,"llr_replies.csv")


df <- read_csv("llr_replies.csv")

d <- df %>%
  select(screen_name,text) %>%
  unnest_tokens(word,text) %>%
  mutate(word = removeWords(word,c("larsloekke","lars","løkke","rasmussen","dit","så","t.co",tm::stopwords("da")))) %>%
  filter(word !="") %>%
  count(word) %>%
  arrange(desc(n))

wordcloud(d$word,d$n,min.freq = 15,colors = scico(n=30,palette = 'lajolla')[10:30] )  

