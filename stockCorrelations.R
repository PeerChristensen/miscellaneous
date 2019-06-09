#Correlation of the US stock market indices from 2010 to 2017
#Peer Christensen

#install.packages("quantmod")
#install.packages("timeSeries")
library(quantmod)
library(timeSeries)
library(ggplot2)

#SP500, Dow Jones, Nasdaq 100, Nasdaq composite & Russell 2000
getSymbols(c("^GSPC","^DJI","^NDX","^IXIC","^RUT"),from="2010-01-01",to="2017-09-10",src="yahoo",class=ts)
df=data.frame(GSPC[,4],DJI[,4],NDX[,4],IXIC[,4],RUT[,4])
names(df) = gsub(pattern = ".Close", replacement = "", x = names(df))
df=scale(df)
cor(df)
Date=as.Date(row.names(df))
df=data.frame(Date,df)

ggplot(df, aes(Date, y = value, colour = variable)) + 
  geom_line(aes(y = GSPC, col = "GSPC")) + 
  geom_line(aes(y = DJI, col = "DJI")) +
  geom_line(aes(y = NDX, col = "NDX")) +
  geom_line(aes(y = IXIC, col = "IXIC")) +
  geom_line(aes(y = RUT, col = "RUT")) +
  ylab("Close value") +
  ggtitle("US stock Indices 2010-2017") +
  labs(color='Index') 
  