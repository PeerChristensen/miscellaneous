library(leaflet)
library(plyr)
library(data.table)
library(stringr)
library(mapview)
library(png)

setwd("/Users/peerchristensen/Desktop/Data")
df=read.csv2("graffiti_all.csv",stringsAsFactors =F)

# appears to have incorrect coordinates
#might be latitude 37.72341 should be 37.92341?
df=df[df$Title!="The Greek health system",] 

setnames(df, old=c("Language..English..Greek..French..German.",
                   "Text.in.English..if.not.text.blank.",
                   "Pictorial..verbopictorial",
                   "Name.of.the.creator"), new=c("Language","Text","Type","Creator"))

df$Longitude=as.numeric(str_split_fixed(df$Latitude, ",", 2)[,1])
df$Latitude=as.numeric(str_split_fixed(df$Latitude, ",", 2)[,2])

img="https://upload.wikimedia.org/wikipedia/commons/8/81/Graffiti_London.jpg"
#img="https://image.ibb.co/hFcTsm/image.jpg" solution to hosting images online
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(lng=df$Longitude, 
                    lat=df$Latitude, 
                    popup=paste(popupImage(img, src = "remote"),"<br>",
                                "Title: ",paste("<a href=",img,">",df$Title,"</a>"),"<br>",
                                "Creator: ",df$Creator,"<br>",
                                "Type: ",df$Type,"<br>",
                                "Text: ",paste("'",df$Text,"'")),
                    clusterOptions = markerClusterOptions(),
                    label = df$Title) %>%
  addMiniMap()
m  # Print the map
