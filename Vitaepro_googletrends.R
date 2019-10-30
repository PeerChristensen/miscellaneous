### mapping google searches for 'stroke' by region in Malaysia

library(GADMTools)
library(sf)
library(gtrendsR)
library(sp)
library(raster)
library(tmap)
library(ggthemes)

vp <- gtrends(c("vitaepro"),
                      gprop = "web",
                      time  = "2017-01-01 2019-08-30",
                      geo   = c("DK"))

plot(vp) +
  geom_line(size=.8) +
  scale_colour_tableau()

vpreg <- vp$interest_by_region %>%
  mutate(location = recode(location,
                           "Capital Region of Denmark" = "Hovedstaden",
                           "Region Syddanmark" = "Syddanmark",
                           "Region Zealand" = "Sjælland",
                           "North Denmark Region" = "Nordjylland",
                           "Central Denmark Region" = "Midtjylland"),
         hits = log(hits))

myMap <- getData("GADM",country="DK",level=1)

#myMap@data$NAME_1 <- recode(myMap@data$NAME_1,"Trengganu"="Terengganu")

#stroke$location <- recode(stroke$location,"Malacca"="Melaka", "Penang" ="Pulau Pinang", "Labuan Federal Territory"="Labuan","Federal Territory of Kuala Lumpur" = "Kuala Lumpur")

vpMerged  <- sp::merge(myMap,vpreg,by.x = "NAME_1",by.y= "location")

#tmap_mode("view")  # for an interactive leaflet map
tmap_mode("plot") # for a static map

tm_shape(vpMerged) +
  tm_polygons("hits") +
  tm_layout(title = "Heat Map of Search Queries",
            title.position = c("center","top")) +
  tm_text("NAME_1", auto.placement = TRUE,size = .7) +
  tm_view(set.zoom.limits = c(6,9)) +
  tm_basemap(NULL)

# interest over time
vptime <- vp$interest_over_time

#1
vptime %>% 
  ggplot(aes(x=as.Date(date),y=hits)) + 
  geom_line() + 
  scale_x_date(date_breaks = "2 years",date_labels = "%Y")
#2

vptime %>% 
  ggplot(aes(x=date,y=hits)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "months",date_labels = "%m")

##########

dk <- getData("GADM",country="NO",level=1)

nm <- gtrends(c("sommerferie"),
              gprop = "web",
              time  = "all",
              geo   = c("NO"))

nm$interest_over_time$date=as.Date(nm$interest_over_time$date)
nm$interest_over_time=nm$interest_over_time %>% filter(date > "2010-01-01")

nmReg <- nm$interest_by_region

nmReg$location <- recode(nmReg$location,"Region Zealand"="Sjælland",
                         "Capital Region of Denmark" = "Hovedstaden",
                         "Central Denmark Region" = "Midtjylland",
                         "North Denmark Region" = "Nordjylland",
                         "Region Syddanmark" = "Syddanmark")

nmMerged  <- sp::merge(dk,nmReg,by.x = "NAME_1",by.y= "location")


tmap_mode("view")  # for an interactive leaflet map
#tmap_mode("plot") # for a static map

tm_shape(nmMerged) +
  tm_polygons("hits") 
#tm_text("NAME_1",group = "hits")

################
# cities

stroke <- gtrends(c("stroke"),
                  gprop = "web",
                  # time  = "2004-01-01 2019-03-31",
                  #time  = "all",
                  geo   = c("MY"))

stroke <- stroke$interest_by_city

myMap <- getData("GADM",country="MYS",level=2)

#myMap@data$NAME_1 <- recode(myMap@data$NAME_1,"Trengganu"="Terengganu")

#stroke$location <- recode (stroke$location,"Malacca"="Melaka", "Penang" ="Pulau Pinang", "Labuan Federal Territory"="Labuan","Federal Territory of Kuala Lumpur" = "Kuala Lumpur")

strokeMerged  <- sp::merge(myMap,stroke,by.x = "NAME_2",by.y= "location")

tmap_mode("view")  # for an interactive leaflet map
#tmap_mode("plot") # for a static map

tm_shape(strokeMerged) +
  tm_polygons("hits") +
  tm_layout(title = "Heat Map of Search Queries in Each State of Malaysia",
            title.position = c("center","top")) +
  tm_text("NAME_2", auto.placement = TRUE,size = .7) +
  tm_view(set.zoom.limits = c(6,9)) +
  tm_basemap(NULL)
