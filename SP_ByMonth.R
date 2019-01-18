# Are there significantly patterns in the stock market movements by month?

#install.packages("quantmod")
#install.packages("timeSeries")
library(quantmod)
#library(timeSeries)
library(ggplot2)
#library(lubridate)
library(plyr)
library(lme4)
library(lmerTest)

#Data: SP500 index from 1950-2017
getSymbols(c("^GSPC"),from="1950-01-01",to="2017-01-01",src="yahoo",class=ts)
plot(GSPC[,4])

#monthly returns
returns=periodReturn(GSPC,period='monthly',subset='1950::')
plot(returns)
hist(df$monthly.returns)

#decompose time series
tsReturns=decompose(ts(returns,frequency=12))
plot(tsReturns)


#create data frame and add months as factor variable
df=as.data.frame(returns)
df$Month=factor(rep(month.name,length(returns)/12),levels = month.name)

#prepare data for a barchart with error bars 
mnts=ddply(df,"Month",summarize,
           Mean=mean(monthly.returns),
           sem=sd(monthly.returns)/sqrt(length(monthly.returns)))

#plot
mntsBar=ggplot(mnts, aes(x=Month, y=Mean))+
  geom_bar(stat="identity",fill="#E69F00",colour="gray30")+
  geom_errorbar(aes(ymin=Mean-sem, ymax=Mean+sem), width=.2,
                position=position_dodge(.9))+
  ggtitle("S&P 500 monthly average returns from 1950 to 2017")+
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
mntsBar


#regression model 1: Do certain months deviate significantly from zero?
#Here, -1 means no intercept
#Note that the output is identical to the mnts dataframe
mod1=lm(monthly.returns~Month-1,data=df)
summary(mod1)

#Question 1: Is there any merit to the saying "sell in May and stay away"?
#regression model 2a: We set May to be the reference level
df$Month=relevel(df$Month,ref=5)
mod2a=lm(monthly.returns~Month,data=df)
summary(mod2a)

#regression model 2b: comparing May to the mean
df$May=NA
#levels(df$May)[levels(df$Month)!="May"] <- "notMay"
df$May[df$Month=="May"]="May"
df$May[df$Month != "May"]="notMay"
mod2b=lm(monthly.returns~May,data=df)
summary(mod2b)

#Question 2: are the May-October period different from November-April
#create two factor levels
df$season=NA
df$season[df$Month %in% month.name[5:10]]="Summer"
df$season[df$Month %in% month.name[c(1:4,11:12)]]="Winter"

mnts2=ddply(df,"season",summarize,
           Mean=mean(monthly.returns),
           sem=sd(monthly.returns)/sqrt(length(monthly.returns)))

#plot
mnts2Bar=ggplot(mnts2, aes(x=season, y=Mean))+
  geom_bar(stat="identity",fill="#E69F00",colour="gray30")+
  geom_errorbar(aes(ymin=Mean-sem, ymax=Mean+sem), width=.2,
                position=position_dodge(.9))+
  ggtitle("S&P 500 monthly average returns from 1950 to 2017")+
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
mnts2Bar

#regression model 3: comparing seasons
mod3=lm(monthly.returns~season,data=df)
summary(mod3)

#regression model 4: Month as random effect
mod4=lmer(monthly.returns~(1|Month)+season,data=df)
summary(mod4)

#bayesian version
library(rstanarm)
mod5=stan_lmer(monthly.returns~(1|Month)+season,data=df)
summary(mod5)

