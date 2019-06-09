# lingtypology demo

install.packages("lingtypology", dependencies = TRUE)
library(lingtypology)

#where is Michif spoken?
country.lang("Michif")

#Data about a language (e.g. Gooniyandi)
gooniyandi = data.frame(subset(glottolog.original,language=="Gooniyandi")) 

#which languages are spoken in Australia? - "Sweden" returns NA(??)
lang.country("Australia")
map.feature(lang.country("Australia"))

#include a feature, e.g. verbal person marking (feature 100A)
myLanguages=c("Nyulnyul", "Warrwa", "Guugu Yimidhirr","Warlpiri","Gooniyandi")
myFeatures=c("accusative","unknown","neutral","unknown","accusative")
map.feature(myLanguages,myFeatures)

#what if you are documenting a language and want to map alternative coordinates?
#adding zoom control, zoom level, minimap and pop-up text
map.feature("Gooniyandi",
            label="Gooniyandi", 
            minimap=T, 
            zoom.control=T, 
            zoom.level=3,
            popup="You can add additional info here <br>another line with info",
            latitude = -19, 
            longitude = 125)

#pop-up video
video="https://media.spreadthesign.com/video/mp4/13/48600.mp4"
video= paste("<video width='200' height='150' controls> <source src='",
                         as.character(video),
                         "' type='video/mp4'></video>", sep = "")
map.feature("Gooniyandi",popup=video)

#changing map types
#see all types here: https://leaflet-extras.github.io/leaflet-providers/preview/index.html
map.feature("Swedish", tile = "Thunderforest.OpenCycleMap")
map.feature("Swedish", tile =c("OpenTopoMap","Stamen.Watercolor"),control=T)
map.feature("Swedish", tile ="NASAGIBS.ViirsEarthAtNight2012",zoom.level=5)
map.feature("Swedish", tile ="Thunderforest.SpinalMap",zoom.level = 5)

# map of Khoisan languages with density contour
map.feature(lang.aff("Khoisan"),density.estimation = 1)
#area only
map.feature(lang.aff("Khoisan"),density.points = FALSE)

# mapping Bantu and Khoisan languages
language=lang.aff(c("Khoisan","Bantu"))
family[grepl("Bantu",aff.lang(language))==T]="Bantu"
family[grepl("Khoisan",aff.lang(language))==T]="Khoisan"
africa=data.frame(language,family)
#let's add latitude and longitude
africa$long=long.lang(africa$language)
africa$lat=lat.lang(africa$language)
#getting rid of row names
rownames(africa) <- c()

#density contour plots
map.feature(africa$language,
            features=africa$family,
            longitude = africa$long,
            latitude = africa$lat,
            density.estimation = africa$family)

#now with some modifications
map.feature(africa$language,
            features=africa$family,
            longitude = africa$long,
            latitude = africa$lat,
            density.estimation =africa$family,
            density.longitude.width = 8,
            density.latitude.width = 8,
            color=c("red","blue"),
            density.estimation.opacity=0.3,
            density.estimation.color = c("red","blue"))

#subsetting a data frame with all data with South Africa as country
df = data.frame(glottolog.original)
za = df[df$country=="South Africa",]
#remove rows where all data are NA
ind <- apply(za, 1, function(x) all(is.na(x)))
za <- za[ !ind, ]

#now looking at a data frame with language, affiliation, location and language status
za=za[,c(1,6,12,14)]
#write to html
library(xtable)
Za=xtable(za)
print.xtable(Za, type="html", file="za.html")

#word order feature from WALS
wo <- wals.feature(c("81a"))
head(wo)

map.feature(wo$language,
            features = wo$`81a`,
            latitude = wo$latitude,
            longitude = wo$longitude,
            label = wo$language,
            title = "Word Order",
            control=T,
            zoom.control = T)

