### mapping google searches for 'stroke' by region in Malaysia

library(GADMTools)
library(sf)
library(gtrendsR)
library(sp)
library(raster)
library(tmap)

stroke <- gtrends(c("stroke"),
                  gprop = "web",
                  time  = "2004-01-01 2019-03-31",
                  geo   = c("MY"))

stroke <- stroke$interest_by_region

myMap <- getData("GADM",country="MYS",level=1)

myMap@data$NAME_1 <- recode(myMap@data$NAME_1,"Trengganu"="Terengganu")

stroke$location <- recode (stroke$location,"Malacca"="Melaka", "Penang" ="Pulau Pinang", "Labuan Federal Territory"="Labuan","Federal Territory of Kuala Lumpur" = "Kuala Lumpur")

strokeMerged  <- sp::merge(myMap,stroke,by.x = "NAME_1",by.y= "location")

tmap_mode("view")  # for an interactive leaflet map
#tmap_mode("plot") # for a static map

tm_shape(strokeMerged) +
  tm_polygons("hits") +
  tm_layout(title = "Heat Map of Search Queries in Each State of Malaysia",
            title.position = c("center","top")) +
  tm_text("NAME_1", auto.placement = TRUE,size = .7) +
  tm_view(set.zoom.limits = c(6,9)) +
  tm_basemap(NULL)

##########

dk <- getData("GADM",country="DK",level=1)

nm <- gtrends(c("mallorca"),
                  gprop = "web",
                  #time  = "all",
                  geo   = c("DK"))

nm$interest_over_time$date=as.Date(nm$interest_over_time$date)
nm$interest_over_time=nm$interest_over_time %>% filter(date > "2010-01-01")

nmReg <- nm$interest_by_region

nmReg$location <- recode(nmReg$location,"Region Zealand"="Sjælland",
                         "Capital Region of Denmark" = "Hovedstaden",
                         "Central Denmark Region" = "Midtjylland",
                         "North Denmark Region" = "Nordjylland",
                         "Region Syddanmark" = "Syddanmark")
  
nmMerged  <- sp::merge(za,nmReg,by.x = "NAME_1",by.y= "location")


tmap_mode("view")  # for an interactive leaflet map
#tmap_mode("plot") # for a static map

tm_shape(nmMerged) +
  tm_polygons("hits") +
  #tm_text("NAME_1",group = "hits") +
  tm_markers(text = "NAME_1")
