### mapping google searches for 'stroke' by region in Malaysia

library(GADMTools)
library(sf)
library(gtrendsR)
library(sp)
library(raster)
library(tmap)

stroke <- gtrends(c("stroke"),
                  gprop = "web",
                  #time  = "2004-01-01 2019-03-31",
                  geo   = c("MY"))

stroke <- stroke$interest_by_region

myMap <- getData("GADM",country="MYS",level=1,mask=TRUE)

myMap@data$NAME_1 <- recode(myMap@data$NAME_1,"Trengganu"="Terengganu")

strokeMerged  <- sp::merge(myMap,stroke,by.x = "NAME_1",by.y= "location")

tmap_mode("view")  # for an interactive leaflet map
#tmap_mode("plot") # for a static map

tm_shape(strokeMerged) +
  tm_polygons("hits") +
  tm_layout(title = "Heat Map of Search Queries in Each State of Malaysia",
            title.position = c("center","top")) +
  tm_text("NAME_1", auto.placement = TRUE,size = .7) +
  tm_view(set.zoom.limits = c(5,9))

##########

dk <- getData("GADM",country="DK",level=1)

nm <- gtrends(c("mallorca"),
                  gprop = "web",
                  #time  = "all",
                  geo   = c("DK"))

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
