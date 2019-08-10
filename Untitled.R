#### date parser

library(tidyverse)
library(lubridate)

columns <- tribble(
  ~day,~month,~year,
  1,1,2018,
  1,2,2017,
  29,5,2019
)

columns <- columns %>%
  mutate(date = dmy(paste(day,month,year)))

# in tableau
# 1. load data
# 2. create calculated field, code: str([Day])+","+str([Month])+","+str([Year])
# 3. change to date

write_csv(columns,"/Users/peerchristensen/Desktop/Projects/Scripts/date_test.csv")

#096134235111