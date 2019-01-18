library(rplos)
library(ggplot2)

df=searchplos(q="'Humanities Lab'", fl=c('author',"author_affiliate","journal",'publication_date',"subject"), limit = 5000)[[2]]
df2=searchplos(q="'Lund University'", fl=c('author',"author_affiliate","journal",'publication_date',"subject"), limit = 5000)[[2]]

df3=merge(df,df2,by="author")
df3=df3[1:5]

#create plot
#x=publication year
#y=n publications
df3$publication_date=as.Date(df3$publication_date.x)
hist(df3$publication_date,breaks=25,freq=T)


plot_throughtime(terms = "'???'", limit = 50000) + geom_line(size=2, color='black')
