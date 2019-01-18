library(ISLR)
library(MASS)

#LINEAR DISCRIMINANT ANALYSIS
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
Smarket2005=subset(Smarket,Year==2005)
pred=predict(lda.fit,Smarket2005)
str(pred)
data.frame(pred)[1:5,]
#confusion matrix
table(pred$class,Smarket2005$Direction)
mean(pred$class==Smarket2005$Direction)


#KNN
library(class)
Lags=cbind(Smarket$Lag1,Smarket$Lag2)
train=Smarket$Year<2005
knn.pred = knn(Lags[train,],Lags[!train,],Smarket$Direction[train])
table(knn.pred,Smarket$Direction[!train])
mean(knn.pred==Smarket$Direction[!train])