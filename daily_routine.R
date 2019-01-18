

# daily routine function
my_daily_routine <- function(baby) {
  
  baby %>% 
    dress(clothes = c("socks", "bodysuit"),
          diaper_change = TRUE) %>%
    sleep(n_hours = 2)          %>%
    feed(n_times  = 100, 
         spit_up  = TRUE)       %>%
    learn_R(code_style = "tidy_AF")
}

# example
my_daily_routine(Viktor)



  