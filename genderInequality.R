#Gender bias and inequality

#language data
lang=read.csv2("mw2.csv",stringsAsFactors = F)
lang=subset(lang,lang$country!="")
lang=lang[,c(6,9)]
lang$lg_ineq=as.numeric(lang$lg_ineq)

#Gender inequality data
ineq=read.csv2("genderInequalityIndex_2015.csv",header=T)
ineq=ineq[,1:2]
ineq=na.omit(ineq)
colnames(ineq)=c("country","gii")

#Merge data
df=merge(lang, ineq,by="country")

# Explore the data
df
plot(lg_ineq~gii,data=df)
text(df$gii, df$lg_ineq, labels = df$country,pos=4,cex=0.5)
     
#Correlating measures of gender inequality
cor(df[,2],df[,3])

#Linear regression
summary(lm(lg_ineq~gii,data=df))

#In case we want to remove most obvious outliers
#df=subset(df,abs(lg_ineq) < 0.9)

#world map of gender inequality index


#world map of linguistic gender inequality
library(rworldmap)
library(ggplot2)
worldMap <- map_data(map="world")
colnames(lang)[1]="region"
lgiDF=merge(worldMap,lang,by="region")

lgiMap <- ggplot() + 
  theme(legend.position="none") +
  geom_map(data=lgiDF, map=worldMap, aes(map_id=region, x=long, y=lat, fill=lg_ineq)) +
  scale_fill_gradient(low = "green", high = "darkred", guide = "colourbar") +
  coord_equal()
lgiMap

#world map of gender inequality index
giiDF=merge(x=worldMap,y=ineq,by.x="region",by.y="country")

giiMap <- ggplot() + 
  theme(legend.position="none") +
  geom_map(data=giiDF, map=worldMap, aes(map_id=region, x=long, y=lat, fill=gii)) +
  scale_fill_gradient(low = "green", high = "darkred", guide = "colourbar") +
  coord_equal()
giiMap


#another way to draw a world map
mapWorld <- borders("world", colour="black", fill="gray50") # create a layer of borders
map <- ggplot() + mapWorld


