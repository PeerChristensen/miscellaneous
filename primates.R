# Primates data

library(DAAG)
library(jtools)
library(magrittr)
library(tidyverse)

df = primates

# brain weight ~ body weight

df %>% 
  ggplot(aes(Bodywt,Brainwt)) + 
  geom_point(size=5) + 
  annotate("text",
           size  = 5,
           x     = df$Bodywt+5,
           y     = df$Brainwt-30,
           label = row.names(df)) +
  theme_apa()

# ratio : brainwt / bodywt

df %<>%
  mutate(species = row.names(df),
         ratio   = Brainwt/Bodywt) %>%
  arrange(ratio) %>%
  mutate(order = row_number())

df %>%
ggplot(aes(x=order,y=ratio,fill=factor(order))) + 
  geom_col(show.legend = FALSE) +
  scale_x_continuous(labels = df$species,
                     breaks = df$order) +
  theme_apa() +
  scale_fill_viridis_d(option="B",df$species) 

