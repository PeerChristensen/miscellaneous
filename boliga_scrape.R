

library(tidyverse)
library(rvest)
library(lubridate)

#### Til salg

url <- "https://www.boliga.dk/resultat?sort=daysForSale-a&daysForSaleMin=0&daysForSaleMax=1&priceMin=15000000"

html <- read_html(url)

output_1 <- html %>%
  html_nodes("[class='primary-value']") %>%
  html_text()

Pris <- output_1[c(FALSE,TRUE)]
Type <- output_1[c(TRUE,FALSE)]

output_2 <- html %>% 
  html_nodes("[class='secondary-value']") %>%
  html_text()

Adresse <- output_2[c(TRUE,FALSE)]

output_3 <- html %>%
  html_nodes("[class='house-list-item']") %>%
  html_attr("href")

Link <- paste0("https://www.boliga.dk",output_3)

df1 <- tibble(Adresse,Pris,Type)

#### Solgte

url <- "https://www.boliga.dk/salg/resultater?searchTab=1&priceMin=15000000&salesDateMin=2019&salesDateMax=2019&sort=date-d&page=1"

html <- read_html(url)

df2 <- html %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]] %>%
  .[1:4] %>%
  tibble() %>%
  pull(.) %>%
  mutate_all(str_remove_all,"Adresse |Købesum |Salgsdato |Boligtype |")

names(df2)    <- c("Adresse","Købesum","Salgsdato","Boligtype")
df2$Købesum   <- str_replace_all(df2$Købesum,"\\.","") %>% as.numeric()
df2$Salgsdato <- dmy(df2$Salgsdato)
df2$Boligtype <- str_extract(df2$Boligtype,"(\\w+)")

df2 %>% 
  arrange(desc(Salgsdato)) %>%
  filter(Salgsdato > today()-1)

 
