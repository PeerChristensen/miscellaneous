# language learning in Europe
# data from eurostat

library(eurostat)

query <- search_eurostat("language", type = "table")
query

dat <- get_eurostat(id="tps00056", time_format="num")
head(dat)

country = dat$geo

dat <- label_eurostat(dat)
dat$country = country
plot_title = "Average number of foreign languages learned per pupil at ISCED level 2"
  
dat = dat %>% 
  filter (indic_ed == "Average number of foreign languages learned per pupil at ISCED level 2") %>%
  select(-indic_ed, -geo) %>%
  filter(!country %in% c("EU27","EU28"))

d = dat %>% 
  filter(time > 2007) %>%
  group_by(time) %>%
  top_n(7, values) %>%
  ungroup() %>%
  arrange(time, -values) %>%
  mutate(order = rev(row_number()))
 
d %>%
  ggplot(aes(order, values, fill = values)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~time, scales = "free") +
  scale_x_continuous(
    breaks = d$order,
    labels = d$country) +
  ggtitle(plot_title) +
  coord_flip() +
  scale_fill_viridis_c(option = "B",begin = .3,end = .8)

nordic = dat %>%
  filter(country %in% c("DK","SE","NO","FI"))
  
nordic %>% spread(-time,values)

nordic %>%
  filter(country %in% c("DK","SE","NO","FI"))  %>%
  ggplot(aes(x=time, y=values, colour=country)) +
  geom_line(size=1, alpha=.6) +
  scale_x_continuous(breaks = nordic$time,
                     labels = as.character(nordic$time)) +
  scale_colour_viridis_d() +
  theme_apa()

### map
m <- get_eurostat(id="tps00056", time_format="num") %>%
  filter(time == 2012) %>%
  mutate(cat = cut_to_classes(values, n=7, decimals=1))

mapdata <- merge_eurostat_geodata(m,
                                  resolution = "20")
