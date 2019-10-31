
dataf =read.table("RESULTS.txt",header=T,sep="\t",stringsAsFactors=FALSE)


table(dataf[,c("Music", "ANSWER")])

#               ANSWER
#                0   1
# Linguistic    63 105
# Music         72  96


table(dataf[dataf$ANSWER==1, c("Music")])/table(dataf[, c("Music")])
 
# Linguistic      Music 
# 0.6250000  0.5714286


dataf.aggr=with(dataf,aggregate(list(answer=ANSWER),by=list(name=Name,music=Music),mean,na.rm=T))

t.test(dataf.aggr[,"answer"]~dataf.aggr[,"music"],paired=T)
 
# Paired t-test
# 
# data:  dataf.aggr[, "answer"] by dataf.aggr[, "music"]
# t = 1.1023, df = 41, p-value = 0.2767
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.04457427  0.15171713
# sample estimates:
#   mean of the differences 
# 0.05357143

plot(table(dataf[,c("Music", "ANSWER")]), col=c("slategray3","limegreen"),
     main = "Music Tasks/ Linguistic Tasks", xlab = "Condition", ylab = "Answer")
 



# H1. Through tasks T1-T4, more contrastive tasks will show better results than less contrastive tasks.

table(dataf[,c("Contrastiveness", "ANSWER")])

#                     ANSWER
# Contrastiveness      0   1
# Less-Conrastive     99  69
# More-Contrastive    36 132



table(dataf[dataf$ANSWER==1, c("Contrastiveness")])/table(dataf[, c("Contrastiveness")])


#  Less-Conrastive   More-Contrastive 
#  0.4107143         0.7857143 


# Of the correct answers, 78% were more-contrastive, while 41% were less-contrastive. This means that
# H1 is supported. 


plot(table(dataf[,c("Contrastiveness", "ANSWER")]), col=c("slategray3","limegreen"),
     main = "More-Contrastive/ Less-Contrastive", xlab = "Condition", ylab = "Answer")




dataf.aggrH1=with(dataf,aggregate(list(answer=ANSWER),by=list(name=Name,contrastiveness=Contrastiveness),mean,na.rm=T))

t.test(dataf.aggrH1[,"answer"]~dataf.aggrH1[,"contrastiveness"],paired=T)

# Paired t-test
# 
# data:  dataf.aggrH1[, "answer"] by dataf.aggrH1[, "contrastiveness"]
# t = -8.0026, df = 41, p-value = 6.581e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4696354 -0.2803646
# sample estimates:
#   mean of the differences 
# -0.375 


# H2 For the music tasks (T1-T2), the unimodal representamina can be expected to be matched to its 
# corresponding object with more frequency than cross-modal representamina.  

modal = dataf[dataf$Music=="Music",]

table(modal[,c("Modality", "ANSWER")])

#                 ANSWER
#                  0  1
# Cross-Modal     43 69
# Unimodal        29 27


table(modal[modal$ANSWER==1, c("Modality")])/table(modal[, c("Modality")])
 
# Cross-Modal       Unimodal 
# 0.6160714         0.4821429 


plot(table(modal[,c("Modality", "ANSWER")]), col=c("slategray3","limegreen"),
           main = "Unimodal/ Cross-Modal", xlab = "Condition", ylab = "Answer")

t.test(subset(dataf, Modality == "Cross-Modal")[,14],subset(dataf, Modality =="Unimodal")[,14], paired = F)

t.test()

modal.aggrH2=with(modal,aggregate(list(answer=ANSWER),by=list(name=Name,modality=Modality),mean,na.rm=T))

t.test(modal.aggrH2[,"answer"]~modal.aggrH2[,"modality"],paired=T)




# H3 For the music tasks (T1-T2), Swedish L1 speakers are expected to have better results than Chinese
# L1 speakers. 

table(modal[,c("L1", "ANSWER")])

#            ANSWER
# L1         0  1
# Chinese    38 46
# Swedish    34 50

table(modal[modal$ANSWER==1, c("L1")])/table(modal[, c("L1")])

# Chinese     Swedish 
# 0.5476190   0.5952381

# Here we can see that both Swedish and Chinese participants performed similarly in the music tasks, where
# 54% of L1 Chinese speakers associated the representamina to the expected object, while 59% of L1 Swedish 
# participants associated the representamina to the expected object. This is not in line with what was 
# expected in the hypothesis. 

plot(table(modal[,c("L1", "ANSWER")]),col=c("slategray3","limegreen"),
     main = "Music Tasks", xlab = "L1", ylab = "Answer")



modal.aggrH3=with(modal,aggregate(list(answer=ANSWER),by=list(name=Name,l1=L1),mean,na.rm=T))

t.test(modal.aggrH3[,"answer"]~modal.aggrH3[,"l1"],paired=T)

# Paired t-test
# 
# data:  modal.aggrH3[, "answer"] by modal.aggrH3[, "l1"]
# t = -0.60788, df = 20, p-value = 0.5501
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2110263  0.1157882
# sample estimates:
#   mean of the differences 
# -0.04761905 


###

table(modal[,c("Image", "ANSWER")])


#         ANSWER
#           0  1
# Image    33 51
# Words    39 45

table(modal[modal$ANSWER==1, c("Image")])/table(modal[, c("Image")])

# Image     Words 
# 0.6071429 0.5357143 

plot(table(modal[,c("Image", "ANSWER")]), col=c("slategray3","limegreen"),
     main = "Image/ Word", xlab = "Condition", ylab = "Answer")

modal.aggrHn=with(modal,aggregate(list(answer=ANSWER),by=list(name=Name,image=Image),mean,na.rm=T))

t.test(modal.aggrHn[,"answer"]~modal.aggrHn[,"image"],paired=T)

# Paired t-test
# 
# data:  modal.aggrHn[, "answer"] by modal.aggrHn[, "image"]
# t = 1.2323, df = 41, p-value = 0.2249
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.04563306  0.18849021
# sample estimates:
#   mean of the differences 
# 0.07142857

# H4A. For the fictive words tasks (T3), L1 Chinese speakers are expected to have similar results to 
# L1 Swedish speakers. 


fictive = dataf[dataf$Task=="T3",]

table(fictive[,c("L1", "ANSWER")])


#            ANSWER
# L1          0   1
# Chinese    17  25
# Swedish    11  31

table(fictive[fictive$ANSWER==1, c("L1")])/table(fictive[, c("L1")])
 
# Chinese     Swedish 
# 0.5952381   0.7380952

plot(table(fictive[,c("L1", "ANSWER")]), col=c("slategray3","limegreen"),
     main = "Fictive Words", xlab = "L1", ylab = "Answer")



fictive.aggrH4a=with(fictive,aggregate(list(answer=ANSWER),by=list(name=Name,l1=L1),mean,na.rm=T))

t.test(fictive.aggrH4a[,"answer"]~fictive.aggrH4a[,"l1"],paired=T)

# Paired t-test
# 
# data:  fictive.aggrH4a[, "answer"] by fictive.aggrH4a[, "l1"]
# t = -2.3355, df = 20, p-value = 0.03005
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2704509 -0.0152634
# sample estimates:
#   mean of the differences 
# -0.1428571 


# H4B. For the ideophones tasks (T4), L1 Swedish speakers are expected to have better results than 
# L1 Chinese speakers.

ideophone = dataf[dataf$Task=="T4",]

table(ideophone[,c("L1", "ANSWER")])

#            ANSWER
# L1         0  1
# Chinese   18 24
# Swedish   17 25

table(ideophone[ideophone$ANSWER==1, c("L1")])/table(ideophone[, c("L1")])

# Chinese   Swedish 
# 0.5714286 0.5952381

plot(table(ideophone[,c("L1", "ANSWER")]), col=c("slategray3","limegreen"),
     main = "Ideophones", xlab = "L1", ylab = "Answer")



ideophone.aggrH4b=with(ideophone,aggregate(list(answer=ANSWER),by=list(name=Name,l1=L1),mean,na.rm=T))

t.test(ideophone.aggrH4b[,"answer"]~ideophone.aggrH4b[,"l1"],paired=T)


# H5. Participants are expected to perform better in the fictive words tasks (T3) than ideophones
# tasks (T4). 


ling = dataf[dataf$Music=="Linguistic",]

table(ling[,c("Words", "ANSWER")])

#               ANSWER
#                 0  1
# Fictive Words  28 56
# Ideophones     35 49

table(ling[ling$ANSWER==1, c("Words")])/table(ling[, c("Words")])
 
#  Fictive Words    Ideophones 
#  0.6666667        0.5833333

plot(table(ling[,c("Words", "ANSWER")]), col=c("slategray3","limegreen"),
     main = "Fictive Words/ Ideophones", xlab = "Tasks", ylab = "Answer")



ling.aggrH5=with(ling,aggregate(list(answer=ANSWER),by=list(name=Name,words=Words),mean,na.rm=T))

t.test(ling.aggrH5[,"answer"]~ling.aggrH5[,"words"],paired=T)


# Paired t-test
# 
# data:  ling.aggrH5[, "answer"] by ling.aggrH5[, "words"]
# t = 1.189, df = 41, p-value = 0.2413
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05820644  0.22487311
# sample estimates:
#   mean of the differences 
# 0.08333333 
