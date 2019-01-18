#Peer Christensen
#script for exploring complete swedish data set, mid june 2017
if(!require("pacman")) install.packages("pacman")
pacman::p_load(xtable,plyr,data.table,ggplot2,scales,RColorBrewer)

setwd("/Users/peerchristensen/Desktop")
#NOTE: use ISO encoding when creating data file in ELAN!
df= read.csv2("sweDataFinal.csv",na.strings = c(""))

#some cleaning
df$Words=gsub("\x9a","ö",df$Words)
df$Words=gsub("\x8c","å",df$Words)
df$Words=gsub("\x8a","ä",df$Words)
df$Scope[df$Scope=="Height"&df$Words=="mörkare"]="Brightness"
df$Scope[df$Scope=="Thickness"&df$Words=="ljust"]="Brightness"


#subset - only include rows with a Trial, Part and Gesture value
df=df[!is.na(df$Trial) & !is.na(df$Part) & !is.na(df$Gesture) ,]
#df=subset(df,df$Scope!="Other")
dfG=df[df$Gesture==1,] #create gesture dataset
df=subset(df,df$Scope!="none")
df=droplevels(df)

# Which words were used and how frequently?
tab=sort(table(df$Words),decreasing =T)
tab=tab[tab>4]
labs=names(tab)
barplot(tab,las=2,ylim = c(0,120))
title("Words used in Swedish")

# Which words were most common with gestures?
tabG=sort(table(dfG$Words),decreasing =T)
tabG=tabG[tabG>4]
labsG=names(tabG)
barplot(tabG,las=2,ylim = c(0,70))
title("Words used in Swedish with gesture")

# Which words were most common without gestures?
dfNoG=df[df$Gesture==0,]
tabNoG=sort(table(dfNoG$Words),decreasing =T)
tabNoG=tabNoG[tabNoG>4]
labsNoG=names(tabNoG)
barplot(tabNoG,las=2,ylim = c(0,70))
title("Words used in Swedish without gesture")

# Words in speech grouped by metaphor
dfM=df[,c(11,13)]
dfM=dfM[dfM$Scope!="none",]
dt=data.table(na.omit(dfM))
dt[,Freq := .N, by=Words]
#for 20 most common words, in order
dt=unique(dt)
dt=dfx[order(-Freq)]
dt=dt[1:15,]

m=ggplot(dt,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Scope)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Swedish grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  scale_fill_discrete(name = "Metaphor") +
  theme_minimal() 
m

# Words in speech grouped by PoS
df$PoS=revalue(df$Words, c("ljusare"="Comparative","lägre"="Comparative",
                            "högre"="Comparative","mörkare"="Comparative",
                            "ljus"="Adjective","ljust"="Adjective","ljusa"="Adjective",
                            "mörk"="Adjective","mörka"="Adjective","mörkt"="Adjective",
                            "hög"="Adjective","högt"="Adjective","höga"="Adjective",
                            "låg"="Adjective","lågt"="Adjective","högre upp"="ADV",
                            "högt upp"="ADV","upp"="ADV","över"="PREP",
                           "under"="PREP","djup"="Adjective","första"="REF",
                           "andra"="REF","ner"="ADV"))
dfP=df[,c(11,13,15)]
dfP=dfP[dfP$Scope!="none",]
dt2=data.table(na.omit(dfP))
dt2[,Freq := .N, by=Words]
dt2=unique(dt2)
dt2=dt2[order(-Freq)]
dt2=dt2[1:15,]
p=ggplot(dt2,aes(x=reorder(Words,Freq),y=Freq)) + 
  geom_bar(stat="identity",aes(fill=PoS)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Swedish grouped by PoS") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal() 
p


#quick table of metaphor and gesture dimension
round(prop.table(table(df$Dimension,df$Scope),2),2)

#plot
fTabSweSp=ftable(df$File,df$Scope)
wt=rowSums(fTabSweSp)
pTabSweSp=round(prop.table(as.matrix(fTabSweSp),1),2)
dfSweSp=data.frame(pTabSweSp,wt)
dfSweSp$Language="Swedish"

summarySweSP=ddply(dfSweSp,"Language",summarise,
                 H= weighted.mean(Height,wt), 
                 B=weighted.mean(Brightness,wt),
                 Hse=sqrt(wtd.var(Height,wt))/sqrt(nrow(dfSweSp)),
                 Bse=sqrt(wtd.var(Brightness,wt))/sqrt(nrow(dfSweSp)))

summarySweSP <- transform(summarySweSP, Hlower=H-Hse, Hupper=H+Hse,Blower=B-Bse,
                        Bupper=B+Bse)

summarySweSP=melt(summarySweSP,Language=Language)

summarySweSP=data.frame(cbind(summarySweSP[1:2,],
                              summarySweSP[3:4,2:3]))

BarSpeechSwe <- ggplot(summarySweSP, aes(x=variable, y=value, fill=Language)) + 
  geom_bar(position=position_dodge(),stat="identity",colour="black") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label = value, y = 0.3, size = 3)) +
  scale_fill_manual(values="#FFFFD9") +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("H","B"),
                   labels=c("Height", "Brightness")) +
  xlab("Metaphors") +
  ggtitle("Swedish pitch metaphors in speech") +
  theme_minimal() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
BarSpeechSwe

#speech and gesture
dfG=droplevels(dfG)
table(dfG$Scope,dfG$Dimension)
round(prop.table(table(dfG$Scope,dfG$Dimension),1),2)

#plot
dfG$Dimension=revalue(dfG$Dimension, c("vert"="Vertical","size"="Other",
                                       "none"="Other","hori"="Other","other"="Other"))
sDatG=ftable(dfG$File,dfG$Scope,dfG$Dimension)
#w=rowSums(sDatG)
#sDatG=prop.table(sDatG,1)
#sDatG=data.frame(sDatG,w)

df=data.frame(sDatG)
df=spread(df,Var3,Freq)
w=df[,3]+df[,4]
df=cbind(df,w)
df[,3]=df[,3]/df[,5]
df[,4]=df[,4]/df[,5]
df$Other[!is.finite(df$Other)]=0
df$Vertical[!is.finite(df$Vertical)]=0
df=df[,-1]
#df=gather(df,Dimension,Prop,Vertical:Other)
#df=cbind(df[1:75,],df[76:150,])
#df=df[,-c(6,7,8)]

summarySweSG=ddply(df,"Var2",summarise,
                   V= weighted.mean(Vertical,w), 
                   O=weighted.mean(Other,w),
                   Vse=sqrt(wtd.var(Vertical,w))/sqrt(nrow(df)),
                   Ose=sqrt(wtd.var(Other,w))/sqrt(nrow(df)))

summarySweSG1=summarySweSG[,1:3]
summarySweSGSEM=summarySweSG[,c(1,4:5)]
summarySweSG1=gather(summarySweSG1,Dimension,Prop,O:V)
summarySweSGSEM=gather(summarySweSGSEM,semDim,semVal,Ose:Vse)

sumSweSG=cbind(summarySweSG1,summarySweSGSEM[,2:3])
sumSweSG$Dimension=factor(sumSweSG$Dimension)

cols1=c("#FFFFD9","#7FCDBB")
sDatGPlot<- ggplot(sumSweSG, aes(x=Var2, y=Prop, fill=Dimension)) + 
  geom_bar(position=position_dodge(),stat="identity",colour="black") +
  geom_errorbar(aes(ymin=Prop-semVal, ymax=Prop+semVal), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=cols1,guide_legend(title = "Gesture Dimension")) +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1))+
  scale_x_discrete(breaks=c("Height","Brightness","none"),
                   labels=c("Height", "Brightness","None"))+
  xlab("Metaphors") +
  ggtitle("Speech-Gesture Co-expressivity: Swedish Speakers") +
  theme_minimal()+
  theme(plot.title=element_text(lineheight=0.8, hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.title=element_text(face="bold",size=17),
        legend.text=element_text(size=15))
sDatGPlot
