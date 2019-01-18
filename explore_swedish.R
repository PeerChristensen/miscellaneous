#Peer Christensen
#script for exploring complete swedish data set, mid june 2017
if(!require("pacman")) install.packages("pacman")
pacman::p_load(xtable,plyr)

setwd("/Users/peerchristensen/Desktop")
df= read.csv2("swe1.csv",na.strings = c(""))

#subset - only include rows with a Part and Gesture value
df=df[!is.na(df$Part) & !is.na(df$Gesture) ,]
df$Words=gsub("\x9a","ö",df$Words)
df$Words=gsub("\x8c","å",df$Words)
df$Words=gsub("\x8a","ä",df$Words)

# Which words were used and how frequently?
tab=sort(table(df$Words),decreasing =T)
tab=tab[tab>4]
labs=names(tab)
barplot(tab,las=2,ylim = c(0,120))

# Which words were most common with gestures?
dfG=df[df$Gesture==1,]
tabG=sort(table(dfG$Words),decreasing =T)
tabG=tabG[tabG>4]
labsG=names(tabG)
barplot(tabG,las=2,ylim = c(0,70))

# Words in speech grouped by metaphor
dfM=df[,c(11,13)]
dt=data.table(dfM)
dt[,Freq := .N, by=Words]
dt=dt[dt$Freq>4,]
m=ggplot(dt,aes(x=reorder(Words,-Freq))) + 
  geom_bar(aes(fill=Scope)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Words in speech grouped by PoS
df$PoS=revalue(df$Words, c("ljusare"="ADJ-comp","lägre"="ADJ-comp",
                            "högre"="ADJ-comp","mörkare"="ADJ-comp",
                            "ljus"="ADJ","ljust"="ADJ","ljusa"="ADJ",
                            "mörk"="ADJ","mörka"="ADJ","mörkt"="ADJ",
                            "hög"="ADJ","högt"="ADJ","höga"="ADJ",
                            "låg"="ADJ","lågt"="ADJ"))
dfP=df[,c(11,15)]
dt2=data.table(dfP)
dt2[,Freq := .N, by=Words]
dt2=dt2[dt2$Freq>16,]
p=ggplot(dt2,aes(x=reorder(Words,-Freq))) + 
  geom_bar(aes(fill=PoS)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
