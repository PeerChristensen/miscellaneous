
# Peer Christensen
# Script for randomly selecting gesture data for independent coding

# install/load xtable
if(!require("pacman")) install.packages("pacman")
pacman::p_load(xtable)

# load and subset data
setwd("/Users/peerchristensen/Desktop")
df= read.csv2("turFinal.csv",na.strings = c(""))
df=df[!is.na(df$Trial) & !is.na(df$Part) & 
        !is.na(df$Words) &!is.na(df$Gesture),]
dfG=df[df$Gesture==1,]

# number of observations for independent coding
totalRows=nrow(dfG)
subsetRows=round(totalRows/100*15,0) # 15 % of the data

# select observations
set.seed(23862)
rows2code=sample(totalRows,subsetRows,replace=F)
dfCode=dfG[rows2code,c(1,2,5,6,12)] # get rows and cols
dfCode=dfCode[with(dfCode,order(File,Trial,Part)),] #sort data frame

# export data frame as html table
codeTab<-xtable(dfCode)
print.xtable(codeTab, type="html", file="Turkish_data_to_code.html")