
library(GADMTools)
library(sf)
library(gtrendsR)
library(sp)

my <- getData("GADM",country="MYS",level=1) # admin level 1 (region)

stroke <- gtrends(c("stroke"),
                  gprop = "web",
                  time  = "2004-01-01 2019-03-31",
                  geo   = c("MY"))

stroke <- stroke$interest_by_region

myMap <- getData("GADM",country="MYS",level=1)

strokeMerged  <- sp::merge(myMap,stroke,by.x = "NAME_1",by.y= "location")

tmap_mode("view")  # for an interactive leaflet map
tmap_mode("plot") # for a static map

tm_shape(strokeMerged) +
  tm_polygons("hits") +
  tm_layout(title = "Heat Map of Search Queries in Each State of Malaysia",
            title.position = c("center","top")) +
  tm_text("NAME_1", auto.placement = TRUE,size = .7) +
  tm_view(set.zoom.limits = c(5,9))



dk <- getData("GADM",country="DK",level=2)

nm <- gtrends(c("Mallorca"),
                  gprop = "web",
                  time  = "2010-01-01 2019-03-31",
                  geo   = c("DK"))

nmReg <- nm$interest_by_region

nmReg$location <- recode(nmReg$location,"Region Zealand"="Sjælland",
                         "Capital Region of Denmark" = "Hovedstaden",
                         "Central Denmark Region" = "Midtjylland",
                         "North Denmark Region" = "Nordjylland",
                         "Region Syddanmark" = "Syddanmark")
  
nmMerged  <- sp::merge(dk,nmReg,by.x = "NAME_1",by.y= "location")


#tmap_mode("view")  # for an interactive leaflet map
tmap_mode("plot") # for a static map

tm_shape(nmMerged) +
  tm_polygons("hits") +
  #tm_text("NAME_1",group = "hits") +
  tm_markers(text = "NAME_1")
