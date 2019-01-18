#3 reviews
setwd( "/Users/peerchristensen/Desktop")
df=read.csv2("3reviews.csv",sep=",")
df=df[-c(1:3),-1]
df$Review=as.character(df$Review)
autodf=subset(df,grepl("auto",df$Review))

autodf

nrow(autodf)

kortdf=subset(df,grepl("kortbetaling",df$Review))

kortdf

autokortdf=subset(df,grepl("automatisk kortbetaling",df$Review))

autokortdf

betaldf=subset(df,grepl("betaling",df$Review))