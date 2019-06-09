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

#bonus
#subset languages with the word "Cree"
df=glottolog.original$language[grepl("Cree",glottolog.original$language)]

#ggplot of languages
df=glottolog.original
df<-subset(df, (!is.na(df$longitude)) & (!is.na(df$latitude)))
langPlot=ggplot(df, aes(longitude,latitude,color=latitude)) +
  geom_point() +
  theme(legend.position="none") +
  scale_color_viridis(option="inferno")

#a 3d interactive world map with links between languages
# Among many other things, this can be used to map geographical dispersal between related languages

# Here we link:
#1. Danish to the other members of Germanic
#2. All Cree languages with links between every language

library(threejs)

earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"

#a data frame with all Germanic languages and coordinates
language=lang.aff("Germanic")
Danish=data.frame(language,
                   lat=lat.lang(language),
                   long=long.lang(language))

#cleaning the data frame a bit, though it may not be strictly necessary
Danish=subset(Danish,languageD!="Danish")
Danish=na.omit(Danish)
rownames(Danish)=c() #not necessary, but the row names are redundant

# a data frame with Cree languages to bind with the Danish
language=lang.aff("Cree")
Cree=data.frame(language,
                  lat=lat.lang(language),
                  long=long.lang(language))
rownames(Cree)=c()

DanCree=data.frame(rbind(Danish,Cree))

#assigning coordinates to create links between Danish and other Germanic languages
# column 1: latitude location 1
# column 2: longitude location 1
# column 3: latitude location 2
# column 4: longitude location 2
coordsDan=cbind(rep(lat.lang("Danish",nrow(Danish))),
                rep(long.lang("Danish",nrow(Danish))),
                Danish$lat,
                Danish$long)

#bonus: links between all Cree languages
language=lang.aff("Cree")
linkCree=data.frame(expand.grid(l1=language,l2=language))
linkCree=subset(linkCree,l1!=l2) 
linkCree=linkCree[!duplicated(t(apply(linkCree,1,sort))),] #remove repeated pairs
coordsCree=cbind(lat.lang(linkCree$l1),
                 long.lang(linkCree$l1),
                 lat.lang(linkCree$l2),
                 long.lang(linkCree$l2))

coords=rbind(coordsDan,coordsCree)



globejs(img = earth, 
        lat = DanCree$latitude, 
        long =DanCree$longitude,
        arcs=coords,
        arcsColor="gold",
        value=25,
        color="red",
        arcsOpacity = 0.4,
        arcsHeight = 0.3,
        arcsLwd = 4,
        atmosphere=T)
