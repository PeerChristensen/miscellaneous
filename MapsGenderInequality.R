# Mapping the Gender Inequality Index (UN, 2015)

library(rworldmap)
library(ggplot2)
library(RColorBrewer)

ineq=read.csv2("genderInequalityIndex_2015.csv",header=T)
ineq=ineq[,1:2]
ineq=na.omit(ineq)
colnames(ineq)=c("country","gii")

worldMap <- map_data(map="world")

giiDF=merge(x=worldMap,y=ineq,by.x="region",by.y="country")

giiMap <- ggplot() + 
  geom_map(data=giiDF, map=worldMap, aes(map_id=region,x=long, y=lat, fill=gii)) +
  scale_fill_gradient(low = "green", high = "darkred") +
  coord_equal() + 
  xlim(-175, 175) 
giiMap

#add contrast with trans = "log10"
giiMap <- ggplot() + 
  theme(legend.title = element_blank()) +
  geom_map(data=giiDF, map=worldMap, aes(map_id=region,x=long, y=lat, fill=gii)) +
  scale_fill_gradient(low = "green", high = "darkred",trans = "log10") +
  coord_equal() + 
  xlim(-175, 175) 
giiMap

#using RColorBrewer colour palettes
#green to red
cols2=brewer.pal(9,"RdYlGn")
giiMap <- ggplot() + 
  theme(legend.title = element_blank()) +
  geom_map(data=giiDF, map=worldMap, aes(map_id=region,x=long, y=lat, fill=gii)) +
  scale_fill_gradient(low = cols2[9], high = cols2[1]) +
  coord_equal() + 
  xlim(-175, 175) 
giiMap

#zoom in on nordic countries
cols2=brewer.pal(9,"RdYlGn")
countries= aggregate(cbind(long,lat)~region,data=giiDF,FUN=median)
giiMap <- ggplot() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'lightblue', colour = 'black')) +
  geom_map(data=giiDF, map=worldMap, aes(map_id=region,x=long, y=lat, fill=gii),color="white",size=0.2) +
  scale_fill_gradient(low = cols2[9], high = cols2[1],trans = "log10") +
  geom_text(data=countries, aes(long,lat,label=region),size=2) +
  coord_equal() + 
  ylim(50,75) +
  xlim(0,35)
giiMap



