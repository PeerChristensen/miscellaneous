#map of denamark

library(maps)
library(mapdata)
dkMap= map("worldHires","Denmark",xlim=c(6,17),ylim=c(54,58))
data = gtrends(c("feminisme"), gprop = "web", time="all", geo = c("DK"))
