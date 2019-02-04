# FIND AND PREPARE TO INSTALL ALL PACKAGES

# february, 2019

library(tidyverse)

pkgs <- installed.packages()

pkgs <- pkgs[,1] %>% as.character %>% as_tibble()

write_csv(pkgs,"pkgs.csv")

install.packages(pkgs)
