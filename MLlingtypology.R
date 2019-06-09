library(rpart)
library(lingtypology)
library(ggplot2)

wordOrder <- wals.feature(c("81a"))
wordOrder=merge(glottolog.original,wordOrder)
wordOrder$wo=factor(wordOrder$`81a`)

set.seed(43282)
smp_size <- floor(0.7 * nrow(wordOrder))
train_ind <- sample(seq_len(nrow(wordOrder)), size = smp_size)
train <- wordOrder[train_ind, ]
test <- wordOrder[-train_ind, ]

fit <- rpart(wo ~ longitude+latitude,
             method="class", data=train,na.action = na.exclude)

test$pred=predict(fit,test,type="class")
test$correct[test$pred==test$wo]="Correct"
test$correct[test$pred!=test$wo]="Incorrect"
test$correct=factor(test$correct)
probs=predict(fit,test,type="prob")
data.frame(test, pred.prob=attr(fit, "prob"))
table(test$correct)
test$pred.prob=rep(0,nrow(test))
for(i in 1:nrow(test)){
  if(test$pred[i]=="No dominant order"){
    test$pred.prob[i]=probs[i,1]}
  else if(test$pred[i]=="OSV"){
    test$pred.prob[i]=probs[i,2]}
  else if(test$pred[i]=="OVS"){
    test$pred.prob[i]=probs[i,3]}
  else if(test$pred[i]=="SOV"){
    test$pred.prob[i]=probs[i,4]}
  else if(test$pred[i]=="SVO"){
    test$pred.prob[i]=probs[i,5]}
  else if(test$pred[i]=="VOS"){
    test$pred.prob[i]=probs[i,6]}
  else
    {test$pred.prob[i]=probs[i,7]}
}

aggregate(test$pred.prob,by=list(test$correct),FUN=mean)

cat("in percent: ", prop.table(table(test$correct))*100)

fit$variable.importance
cat("Scaled to 100: ", fit$variable.importance/sum(fit$variable.importance)*100)

#wordOrder$pred=predict(fit,wordOrder,type = "class")

#wordOrder$correct[wordOrder$pred==wordOrder$wo]="Correct"
#wordOrder$correct[wordOrder$pred!=wordOrder$wo]="Incorrect"
#wordOrder$correct=factor(wordOrder$correct)

#table(wordOrder$correct)
#cat("in percent: ", prop.table(table(wordOrder$correct))*100)

map.feature(languages=test$language,
            features=test$correct,
            color=c("green","red"),
            minimap=T,
            zoom.control=T,
            popup = paste("actual: ",test$wo,"<br>","predicted: ",test$pred,"<br>","pred. probability: ",round(test$pred.prob,2)))



wo=table(wordOrder$wo)
pred=table(wordOrder$pred)
df=rbind(wo,pred)
df=t(df)
colnames(df)=c("actual","predicted")
library(reshape)
df=melt(df)
ggplot(df,aes(X1,value,fill=X2)) +
  geom_col(position="dodge") + 
  theme_dark() +
  ggtitle("Distribution of actual and predicted word orders") +
  xlab("Word order") +
  ylab("N languages") +
  theme(legend.title=element_blank())

#knn
wordOrder <- wals.feature(c("81a"))
wordOrder=merge(glottolog.original,wordOrder)
wordOrder$wo=factor(wordOrder$`81a`)
wordOrder=na.omit(wordOrder[,c(1,4,5,22)])
set.seed(43282)
smp_size <- floor(0.7 * nrow(wordOrder))
train_ind <- sample(seq_len(nrow(wordOrder)), size = smp_size)
train <- wordOrder[train_ind, ]
test <- wordOrder[-train_ind, ]
#removing factor variable from training and test datasets
train1 <- train[-c(1,4)]
testLanguage=test[,1]
test1 <- test[-c(1,4)]
#storing outcome variable
trainLabs <- train$wo 
testLabs = test$wo
library(class)
testPred <- knn(train = train1, test = test1, cl= trainLabs,k = 5)
#eval
library(gmodels)
CrossTable(x = testLabs, y = testPred,prop.chisq=FALSE)

#map
testLabs=data.frame(testLanguage,testLabs)
map=data.frame(testLabs,testPred)
names(map) <- c("Language","Predicted", "Actual")
map$correct[map$Predicted==map$Actual]="Correct"
map$correct[map$Predicted!=map$Actual]="Incorrect"

map.feature(languages=map$Language,
            features=map$correct,
            color=c("green","red"),
            minimap=T,
            zoom.control=T,
            popup = paste("actual: ",map$Actual,"<br>","predicted: ",map$Predicted))



#multiple knn
wordOrder <- wals.feature(c("81a"))
wordOrder=merge(glottolog.original,wordOrder)
wordOrder$wo=factor(wordOrder$`81a`)
wordOrder=na.omit(wordOrder[,c(1,4,5,22)])
set.seed(43282)
smp_size <- floor(0.7 * nrow(wordOrder))
train_ind <- sample(seq_len(nrow(wordOrder)), size = smp_size)
train <- wordOrder[train_ind, ]
test <- wordOrder[-train_ind, ]
#removing factor variable from training and test datasets
train1 <- train[-c(1,4)]
test1 <- test[-c(1,4)]
testLanguage=test[,1]
trainLanguage=train[,1]
#storing outcome variable
trainLabs <- train$wo 
testLabs = test$wo
testLabels=data.frame(testLanguage,testLabs)
trainLabels=data.frame(trainLanguage,trainLabs)

library(class)
ks=seq(1,31,2)

#test accuracy
test_accuracy = NULL
for (k in ks){
  testPred <- knn(train = train1, test = test1, cl= trainLabs,k = k)
  map=data.frame(testLabels,testPred)
  names(map) <- c("Language","Actual", "Predicted")
  map$correct[map$Predicted==map$Actual]="Correct"
  map$correct[map$Predicted!=map$Actual]="Incorrect"
  test_accuracy = append(test_accuracy,(table(map$correct)/sum(table(map$correct)))[1])
}
#train accuracy
train_accuracy = NULL
for (k in ks){
  trainPred <- knn(train = train1, test = train1, cl= trainLabs,k = k)
  trainDF=data.frame(trainLabels,trainPred)
  names(trainDF) <- c("Language","Actual", "Predicted")
  trainDF$correct[trainDF$Predicted==trainDF$Actual]="Correct"
  trainDF$correct[trainDF$Predicted!=trainDF$Actual]="Incorrect"
  train_accuracy = append(train_accuracy,(table(trainDF$correct)/sum(table(trainDF$correct)))[1])
}

#plot of train-test accuracy
acc=data.frame(ks,train_accuracy,test_accuracy)
plot(acc$ks,acc$test_accuracy,type="b",col="blue",ylim=c(0.5,1),xlab="k",ylab="Accuracy")
lines(acc$ks,acc$train_accuracy,col="red",type="b")
legend("bottom", c("Test","Train"),col=c("blue","red"),lty=c(1,1))
max=subset(acc,test_accuracy==max(test_accuracy))
#show best k's
text(max[,1],max[,3], labels = round(max[,3],2),pos=1)

#regression