
library(rtweet)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(scico)
library(sentimentr)


df_mcd <- search_tweets(
  "mcdonalds AND -filter:replies AND -filter:retweets AND -filter:quotes",
  n = 30000,
  retryonratelimit = TRUE,lang = "en",
  token = bearer_token())

df_mcd[sapply(df_mcd, is.list)] <- lapply(df_mcd[sapply(df_mcd, is.list)],
                                              as.character)
sentiment <- df_mcd %>%
  get_sentences() %>% sentiment_by()

df_mcd$sentiment <- sentiment$ave_sentiment

df_mcd$chain <- "McDonald's"

df_bk <- search_tweets(
  "burger king AND -filter:replies AND -filter:retweets AND -filter:quotes",
  n = 30000,
  retryonratelimit = TRUE,lang = "en",
  token = bearer_token())

df_bk[sapply(df_bk, is.list)] <- lapply(df_bk[sapply(df_bk, is.list)],
                                            as.character)
sentiment <- df_bk %>%
  get_sentences() %>% sentiment_by()

df_bk$sentiment <- sentiment$ave_sentiment

df_bk$chain <- "Burger King"

df <- rbind(df_mcd,df_bk)

df %>%
  ggplot(aes(sentiment,colour =chain)) +
  geom_density()


df_ikea <- search_tweets(
  "ikea AND -filter:replies AND -filter:retweets AND -filter:quotes",
  n = 30000,
  retryonratelimit = TRUE,lang = "en",
  token = bearer_token())

df_ikea[sapply(df_ikea, is.list)] <- lapply(df_ikea[sapply(df_ikea, is.list)],
                                          as.character)
sentiment <- df_ikea %>%
  get_sentences() %>% sentiment_by()

df_ikea$sentiment <- sentiment$ave_sentiment

