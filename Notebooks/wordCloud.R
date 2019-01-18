
sentences<-scan("tekst.txt","character",sep="\n",fileEncoding="ISO-8859-1");
#Replace full stop and comma
sentences<-gsub("\\.","",sentences)
sentences<-gsub("\\,","",sentences)
#Split sentence
words<-strsplit(sentences, " ")
words <- unlist(words)
#Calculate word frequencies
words.freq<-table(words);
wf=sort(words.freq,decreasing =T)
wf=data.frame(wf)
wf$words=as.character(wf$words)
wf=subset(wf,nchar(wf$words) >3)
wf=subset(wf,nchar(wf$words) <20)
wf1=wf[1:50,]
library(RColorBrewer)
cols=brewer.pal(5,"Greens")[4:9]
wordcloud(wf1$words,wf1$Freq,colors=cols)
