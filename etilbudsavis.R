library(dplyr)

base_url = "https://api.etilbudsavis.dk/v2/offers/search?r_lat={r_lat}&r_lng={r_lng}&r_radius={r_radius}&query={query}&offset={offset}&limit={limit}"

query = glue::glue(base_url, query = "aperol",
                 r_lat =  55.6656223,
                 r_lng =  12.6009489,
                 r_radius = 4000,
                 offset = 0,
                 limit = 100)

# simple query

base_url <- "https://api.etilbudsavis.dk/v2/offers/search?query="

search_item <- "vodka"
query = glue::glue("{base_url}{search_item}&r_lat=55.6656223&r_lng=12.6009489&r_radius=2000")

data  =  jsonlite::fromJSON(query)

heading <- data$heading
store <- data$branding$name
price <- data$pricing$price
#quantity_unit <- data$quantity$unit$symbol # only works sometimes
quantity_min <- data$quantity$size$from
from_date <- as.Date(data$run_from)
from_day <- lubridate::wday(as.Date(data$run_from),label=T,abbr=F)
until_date <- as.Date(data$run_till)
until_day <- lubridate::wday(as.Date(data$run_till),label=T,abbr=F)

df <- tibble(heading,store,price,quantity_min,from_date,from_day,until_date,until_day)
df

d <- df %>% filter(price<119)

if (nrow(d) > 0) {
  print("Do something")
}

