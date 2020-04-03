# scrape famous quotes


library(tidyverse)
library(rvest)

pages <- paste0("https://www.goodreads.com/quotes?page=",as.character(1:100))

df <- NULL

for (i in pages) {
 
  html <-  i  %>% xml2::read_html() 
  
  authors <- html %>%
    html_nodes(".authorOrTitle") %>%
    html_text() %>%
    as_tibble() %>%
    filter(str_detect(value,"    ")) %>%
    pull(value) %>% str_split("\\n") %>%
    map(2) %>%
    str_trim()
  
  quotes <- html %>%
    html_nodes(".quoteText") %>%
    html_text() %>%
    str_split("“") %>% 
    map(2) %>%
    str_extract_all("^(.*?)”") %>%
    str_remove_all("\\”")
  
  d <- tibble(authors,quotes)
  
  df <- rbind(df,d)
}

write_csv(df,"famous_quotes.csv")