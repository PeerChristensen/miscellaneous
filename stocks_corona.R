
library(rvest)
library(tidyquant)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp500 <- url %>%
  xml2::read_html() %>%
  html_nodes("#constituents") %>%
  html_table()

sp500 <- sp500[[1]]
#write_csv(sp500,"sp500_table_csv")
#sp500 <- read_csv("sp500_table_csv")

tickers <- sp500$Symbol

stocks_get <- tq_get(tickers, 
                     get  = "stock.prices",
                     from = "2020-02-21",
                     to   = "2020-03-17")

stocks <- stocks_get %>% 
  select(date,symbol,close)

#write_csv(stocks, "stocks_sp500_corona_period.csv")
#stocks <- read_csv("stocks_sp500_corona_period.csv")

df <- stocks %>%
  filter(date == min(date)| date == max(date)) %>% 
  pivot_wider(names_from = date,values_from=close) %>%
  rename(start = `2020-02-21`, end = `2020-03-16`) %>%
  mutate(pct_decrease = (start - end) / start * 100) %>%
  inner_join(sp500,by = c("symbol" = "Symbol")) %>%
  select(Security, symbol, pct_decrease, sector = `GICS Sector`)

df %>%
  group_by(sector) %>%
  summarise(avg = mean(pct_decrease)) %>%
  arrange(desc(avg))

# simple plot
df %>%
  ggplot(aes(sector,pct_decrease, colour = sector)) +
  geom_boxplot(show.legend = F) +
  coord_flip() 

# nicer plot
df %>%
  ggplot(aes(sector,pct_decrease, colour = sector)) +
  geom_boxplot(outlier.shape = NA,size = 1) +
  geom_jitter(width = .2, size = 2.5, alpha = .7) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  coord_flip() +
  ggthemes::scale_colour_tableau("Tableau 20") +
  ggtitle("Stock Price Decrease During the COVID-19 Crisis",
          subtitle = "February 21 - March 16 2020\n") +
  labs(
       caption = "\nSource: Yahoo Finance") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16, face = "italic"),
        plot.caption = element_text(size = 12, face = "italic"))

# -----------------------------------------------

url <- "https://en.wikipedia.org/wiki/NASDAQ-100"

ndx <- url %>%
  xml2::read_html() %>%
  html_nodes("#constituents") %>%
  html_table()

ndx <- ndx[[1]]

ndx_tickers <- ndx$Ticker

ndx_stocks <- tq_get(ndx_tickers,
                      get  = "stock.prices",
                      from = "2020-01-19",
                      to   = lubridate::today())

ndx_stocks <- ndx_stocks %>% 
  select(date,symbol,close)

start <- ndx_stocks %>%
  filter(date == min(date)) %>%
  select(-date) %>%
  rename(start = close)

end <- ndx_stocks %>%
  filter(date == max(date)) %>%
  select(-date) %>%
  rename(end = close)

df <- start %>%
  inner_join(end)

df <- df %>%
  mutate(pct_decrease = (start - end) / start * 100) %>%
  arrange(desc(pct_decrease)) %>%
  left_join(SP500,by = c("symbol" = "Symbol")) %>%
  select(Security, symbol, pct_decrease, sector = `GICS Sector`, industry = `GICS Sub Industry`)

df %>% 
  filter(sector == "Information Technology") %>%
  arrange(desc(pct_decrease))
  
# start <- stocks %>%
#   filter(date == min(date)) %>%
#   select(-date) %>%
#   rename(start = close)
# 
# end <- stocks %>%
#   filter(date == max(date)) %>%
#   select(-date) %>%
#   rename(end = close)
# 
# df <- start %>%
#   inner_join(end)