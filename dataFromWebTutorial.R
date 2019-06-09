# Google Trends tutorial and an example of integrating data with a map

devtools::install_github('PMassicotte/gtrendsR')
library(gtrendsR)
library(maps)
library(ggplot2)

# Using geo data
hurricanes = gtrends(c("Katrina","Harvey","Irma"), gprop = "web",time="all", geo = c("US"))
plot(hurricanes)

harvey = gtrends(c("fidget spinner"), gprop = "web",time="today 12-m", geo = c("US"))
harvey = harvey$interest_by_region
harvey$region = sapply(harvey$location,tolower)
statesMap = map_data("state")
harveyMerged = merge(statesMap,harvey,by="region")

regionLabels <- aggregate(cbind(long, lat) ~ location, data=harveyMerged, 
                          FUN=function(x) mean(range(x)))

harveyPlot=ggplot() +
  geom_polygon(data=harveyMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkred",guide="colorbar") +
  geom_text(data=regionLabels, aes(long, lat, label = location), size=2) +
  theme_bw() +
  labs(title="Google search interest for Hurricane Harvey in each state from the week prior to landfall in the US")
harveyPlot

irma = gtrends(c("Irma"), gprop = "web", time="today 12-m",geo = c("US"))
irma = irma$interest_by_region
irma$region = sapply(irma$location,tolower)
statesMap = map_data("state")
irmaMerged = merge(statesMap ,irma,by="region")

irmaPlot=ggplot() +
  geom_polygon(data=irmaMerged,aes(x=long,y=lat,group=group,fill=hits),colour="white") +
  scale_fill_continuous(low="thistle2",high="darkblue",guide="colorbar") +
  geom_text(data=regionLabels, aes(long, lat, label = location), size=2) +
  theme_bw() +
  coord_fixed(1.3) +
  labs(title="Google search interest for Hurricane Irma in each state from the week prior to landfall in the US") 
irmaPlot
