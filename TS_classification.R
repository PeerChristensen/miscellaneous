#Time series calssification

setwd("/Users/peerchristensen/Desktop")
library(plyr)
library(party)
library(wavelets)
library(class)

## Classification
#The dataset contains 600 examples of control charts 
#Each control chart is a time series with 60 values.
#Six classes:
#  1-100 Normal
#101-200 Cyclic
#201-300 Increasing trend
#301-400 Decreasing trend
#401-500 Upward shift
#501-600 Downward shift
sc <- read.table("synthetic_control.data.txt", header = F, sep = "")
classId <- rep(as.character(1:6), each = 100)
newSc <- data.frame(cbind(classId, sc))

#decision tree
ct <- ctree(classId ~ ., data = newSc,
            controls = ctree_control(minsplit = 20,
                                     minbucket = 5, maxdepth = 5))

pClassId <- predict(ct)
table(classId, pClassId)

# accuracy
sum(classId == pClassId)/nrow(sc)


#################################
## Motion capture data
df=read.csv2("MoClass1.csv")
for (i in seq(1,ncol(df)))
  df[,i] = as.character(df[,i])
for (i in seq(1,ncol(df)))
  df[,i] = as.numeric(df[,i])
plot(df$Time, df$T10.X,type="l")
plot(df$Time, df$T10.Y,type="l")
plot(df$Time, df$T10.Z,type="l")

noise=sample(1:650,12000,replace=T)

df1=data.frame(ts(df$Time),df$T10.Y,df$T10.Z)

#merge noise and data
y=ts(df1$df.T10.Y)
z=df1$df.T10.Z
y=ts(c(y,noise),frequency=1)
z=ts(c(z,noise),frequency=1)
class=c(rep(1,12000),rep(0,12000))
df2=data.frame(y,z,class)
df2=split(df2,1:100)
df2=sample(df2)
df2 <- ldply(sample(df2), data.frame)
df2=df2[,-1]
plot(df2$class[1:500],type="l")

set.seed(3237)
df2$class=factor(df2$class)
ind=sample(1:nrow(df2),0.6*nrow(df2))
train=df2[ind,]
test=df2[-ind,]

#Decisipn tree
ct <- ctree(class ~ ., data = train,
            controls = ctree_control(minsplit = 5,
                                     minbucket = 5, maxdepth = 10))
# accuracy
pclass <- predict(ct,test)
table(test$class, pclass)
sum(test$class == pclass)/nrow(test)

# KNN
ks=seq(1,201,25)

test_accuracy = NULL
for (k in ks){
  testPred <- knn(train = train,test=test,cl=train$class, k=k)
  test_accuracy = append(test_accuracy,sum(diag(table(test$class,testPred)))/sum(table(test$class,testPred)))
}

train_accuracy = NULL
for (k in ks){
  trainPred <- knn(train = train, test = train, cl= train$class,k = k)
  train_accuracy = append(train_accuracy,sum(train$class==trainPred)/nrow(train))
}

#plot of train-test accuracy
acc=data.frame(ks,train_accuracy,test_accuracy)
plot(acc$ks,acc$test_accuracy,type="b",col="blue",ylim=c(0.95,1),xlab="k",ylab="Accuracy")
lines(acc$ks,acc$train_accuracy,col="red",type="b")
legend("bottom", c("Test","Train"),col=c("blue","red"),lty=c(1,1))

#show best test k
max(acc$k[acc$test_accuracy >= 0.99])
text(176,1, labels = paste("k=176\n",round(acc[8,3],4)),pos=1,offset = 1)

knnPred= knn(train = train, test = test, cl= train$class,k = 176)
knnTable1=table(test$class, knnPred)
#accuracy
knnTable1
sum(diag(knnTable1))/sum(knnTable1)

#KNN 2 predictors
df=read.csv2("MoClass1.csv")
for (i in seq(1,ncol(df)))
  df[,i] = as.character(df[,i])
for (i in seq(1,ncol(df)))
  df[,i] = as.numeric(df[,i])
plot(df$Time, df$T10.X,type="l")
plot(df$Time, df$T10.Y,type="l")
plot(df$Time, df$T10.Z,type="l")

df$class=factor(1)
noise=as.numeric(sample(1:650,12000,replace=T))

df2=data.frame(ts(df$Time),df$T10.Y,df$T10.Z,df$class,noise)
#merge noise and data
y=df2$df.T10.Y
z=df2$df.T10.Z
ts1=ts(c(y,noise),frequency=1)
ts2=ts(c(z,noise),frequency=1)

class=c(rep(1,12000),rep(0,12000))
df3=data.frame(ts1,ts2,class)
z=split(df3,1:120)
z=sample(z)
df <- ldply(z, data.frame)
df=df[,-1]
df$class=factor(df$class)

ind=sample(1:nrow(df),0.6*nrow(df))
train=df[ind,]
test=df[-ind,]
ks=seq(1,31,4)

test_accuracy = NULL
for (k in ks){
  testPred <- knn(train = train,test=test,cl=train$class, k=k)
  test_accuracy = append(test_accuracy,sum(diag(table(test$class,testPred)))/sum(table(test$class,testPred)))
}

train_accuracy = NULL
for (k in ks){
  trainPred <- knn(train = train, test = train, cl= train$class,k = k)
  train_accuracy = append(train_accuracy,sum(train$class==trainPred)/nrow(train))
}

#plot of train-test accuracy
acc=data.frame(ks,train_accuracy,test_accuracy)
plot(acc$ks,acc$test_accuracy,type="b",col="blue",ylim=c(0.94,1),xlab="k",ylab="Accuracy")
lines(acc$ks,acc$train_accuracy,col="red",type="b")
legend("bottom", c("Test","Train"),col=c("blue","red"),lty=c(1,1))

#plot decision boundary
require(MASS)

test <- expand.grid(x=seq(min(train[,1]-1), max(train[,1]+1),
                          by=0.1),
                    y=seq(min(train[,2]-1), max(train[,2]+1), 
                          by=0.1))
library(ElemStatLearn)
mod15 <- knn(train, test, train$class, k=15, prob=TRUE)
prob <- attr(mod15, "prob")
require(dplyr)

dataf <- bind_rows(mutate(test,
                          prob=prob,
                          cls="c",
                          prob_cls=ifelse(mod15==cls,
                                          1, 0)),
                   mutate(test,
                          prob=prob,
                          cls="v",
                          prob_cls=ifelse(mod15==cls,
                                          1, 0)),
                   mutate(test,
                          prob=prob,
                          cls="s",
                          prob_cls=ifelse(mod15==cls,
                                          1, 0)))

require(ggplot2)
ggplot(dataf) +
  geom_point(aes(x=x, y=y, col=cls),
             data = mutate(test, cls=mod15),
             size=1.2) + 
  geom_contour(aes(x=x, y=y, z=prob_cls, group=cls, color=cls),
               bins=2,
               data=dataf) +
  geom_point(aes(x=x, y=y, col=cls),
             size=3,
             data=data.frame(x=train[,1], y=train[,2], cls=train$class))

#DWT
library(wavelets)
wtData <- NULL
for (i in 1:nrow(df)) {
  a <- t(df[i,-3])
  wt <- dwt(a, filter = "haar", boundary = "periodic")
  wtData <- rbind(wtData, unlist(c(wt@W, wt@V[[wt@level]])))
}
wtData <- as.data.frame(wtData)
wtData$class=as.factor(class)
ct <- ctree(class ~ ., data = wtData,
            controls = ctree_control(minsplit=1, minbucket=1,
                                     maxdepth=5))

pclass <- predict(ct)
table(class, pclass)

### knn with wavelets
test_accuracy = NULL
for (k in ks){
  testPred <- knn(train = wtData[1:1],test=test,cl=train$class, k=k)
  test_accuracy = append(test_accuracy,sum(diag(table(test$class,testPred)))/sum(table(test$class,testPred)))
}

###
df=df[,3:4]
wtData <- NULL
for (i in 1:nrow(df)) {
  a <- t(df[i, ])
  wt <- dwt(a, filter = "haar", boundary = "periodic")
  wtData <- rbind(wtData, unlist(c(wt@W, wt@V[[wt@level]])))
}
wtData <- as.data.frame(wtData)
wtSc <- data.frame(cbind(clas, wtData))
ct <- ctree(classId ~ ., data = wtSc,
            controls = ctree_control(minsplit=20, minbucket=5, maxdepth = 25))

pClassId <- predict(ct)
table(classId, pClassId)
