# What hip-hop thinks of the 2016 presidential candidates

library(fivethirtyeight)
library(plotly)
library(RColorBrewer)
attach(hiphop_cand_lyrics)

#subset Hillary and Donald
a=hiphop_cand_lyrics[candidate=="Hillary Clinton" |candidate== "Donald Trump",]

#from 1989-2016
bar_tab=prop.table(t(table(a$candidate,a$sentiment)),2)
Sentiment=dimnames((bar_tab))[[1]]
don=bar_tab[,1]
hil=bar_tab[,2]
df=data.frame(Sentiment,don,hil)
hiphop_plot=plot_ly(data=df,type="bar", x = ~Sentiment, y = ~hil,name="Hillary Clinton") %>% 
        layout(barmode="group") %>%
        add_trace(y=~don,name="Donald Trump") %>%
        layout(title="The 2016 Pres. Candidates in Hip-Hop Lyrics (1989-2016)",
                yaxis=list(title="Proportions"))
hiphop_plot

# from 2014
a2=a[a$album_release_date>2013,]
b=prop.table(table(a2$candidate,a2$sentiment),1)
barplot(b,beside=T,legend=F,col=c("orange3","white"),
        main="Sentiments expressed by rappers in song lyrics")
b2=prop.table(table(a2$candidate,a2$sentiment),1)

# Sentiment across time
#Donald
D_dat=subset(hiphop_cand_lyrics,candidate=="Donald Trump")
D_year=dimnames((D_tab))[[1]]
D_tab=t(table(D_dat$sentiment,D_dat$album_release_date))
Dneg=D_tab[,1]
Dneu=D_tab[,2]
Dpos=D_tab[,3]
D_df=data.frame(D_year,Dneg,Dneu,Dpos)
plot_ly(D_df,x=~D_year,y=~Dneg,name="negative",type="scatter",mode="lines") %>%
add_trace(y=~Dneu,name=~"neutral",type="scatter",mode="lines") %>%
add_trace(y=~Dpos,name=~"positive",type="scatter",mode="lines") %>%
layout(title="Donald Trump", yaxis=list(title="N mentions in rap lyrics"),xaxis=list(title="Year"))

#Hillary
H_dat=subset(hiphop_cand_lyrics,candidate=="Hillary Clinton")
H_tab=t(table(H_dat$sentiment,H_dat$album_release_date))
H_year=dimnames((H_tab))[[1]]
Hneg=H_tab[,1]
Hneu=H_tab[,2]
Hpos=H_tab[,3]
H_df=data.frame(H_year,Hneg,Hneu,Hpos)
plot_ly(H_df,x=~H_year,y=~Hneg,name="negative",type="scatter",mode="lines") %>%
        add_trace(y=~Hneu,name=~"neutral",type="scatter",mode="lines") %>%
        add_trace(y=~Hpos,name=~"positive",type="scatter",mode="lines") %>%
        layout(title="Hillary Clinton", yaxis=list(title="N mentions in rap lyrics"),xaxis=list(title="Year"))

