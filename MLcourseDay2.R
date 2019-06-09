# ml course day2

#airbnb exercise, predict price from summary terms
dim(airbnb_summary_terms)
m <- cv.glmnet(y = log(airbnb$price+0.01), 
               x = airbnb_summary_terms, 
               alpha = 1, family = "binomial", nfolds = 5)
#plot(m)
relevant <- predict(m, type = "coefficients", s = m$lambda.1se)[, 1]
relevant <- relevant[relevant != 0]
relevant <- sort(relevant, decreasing = TRUE)
relevant



#bluecross
#On the bluecross dataset. Identify the variables which are driving the target using a LARS model.
#x <- nearZeroVar(bluecross, freqCut = 95/5, saveMetrics = TRUE) 
#x <- subset(x, zeroVar == TRUE)
df=bluecross
df=na.exclude(df)
#df[,-nearZeroVar(df)]
df=downSample(x=df,y=factor(df$y),yname="y")
remove=nearZeroVar(x=df,saveMetrics = T)
remove=subset(remove,nzv==T|zeroVar==T)
df=df[,setdiff(colnames(df),rownames(remove))]

dummyfy <- dummyVars(formula = y ~ ., data = df, fullRank = FALSE,sep="_")
x <- predict(dummyfy, newdata = df)
y=df$y
m=cv.glmnet(y=y,x=x,alpha=1,family="binomial",nfolds=5)


#claims
dataset <- as.data.table(subset(claims, !is.na(primaryconditiongroup)))
## Set up predictive features
predictors <- dataset[year %in% c(1, 2), list(treatments = .N), by = list(memberid, primaryconditiongroup)]
predictors <- dcast.data.table(predictors, memberid ~ primaryconditiongroup, 
                               fun=function(x) sum(x), fill=0L, value.var = "treatments")

dataset <- merge(
  dataset[, list(
    target = sum(year == 3 & primaryconditiongroup %in% "Ingestions and benign tumors")), 
    by = list(memberid)],
  predictors,
  by = "memberid")
dataset$target <- factor(ifelse(dataset$target > 0, "Yes", "No"), levels = c("No", "Yes"))
dataset <- as.data.frame(dataset)
colnames(dataset) <- make.names(tolower(colnames(dataset)))
predictors <- setdiff(names(dataset), c("target","memberid"))
str(dataset)

mydata=dataset[,-1]
trainsamples <- createDataPartition(y = dataset$target, times=1, p = 0.75, list=FALSE)
trainsamples <- unlist(trainsamples)

#m=nnet(x=dataset[trainsamples,-c(1,2)],y=dataset[trainsamples,1],
#       linout = F, size = 3)

m=nnet(target~.,data=mydata[trainsamples,],linout=F,size=5)
pred=prediction(predictions=predict(m, mydata[-trainsamples,]),labels=mydata$target[-trainsamples])
perf=performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)

set.seed(134)
m=train(target~.,data=mydata[trainsamples,],
        method = "nnet",
        metric="Accuracy",
        tuneLength = 2,
        trace = T,
        maxit = 100,
        linout=F,
        tuneGrid= expand.grid(size=c(2,4,7),decay=c(0,0.05,0.1,0.15)),
        trcontrol=trainControl(method="repeatedcv",number=5,repeats=5))

pred=prediction(predictions=predict(m, mydata[-trainsamples,]),labels=mydata$target[-trainsamples])
perf=performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)


#rpart
dataset <- as.data.table(subset(claims, !is.na(primaryconditiongroup)))
## Set up predictive features
predictors <- dataset[year %in% c(1, 2), list(treatments = .N), by = list(memberid, primaryconditiongroup)]
predictors <- dcast.data.table(predictors, memberid ~ primaryconditiongroup, 
                               fun=function(x) sum(x), fill=0L, value.var = "treatments")

dataset <- merge(
  dataset[, list(
    target = sum(year == 3 & primaryconditiongroup %in% "Ingestions and benign tumors")), 
    by = list(memberid)],
  predictors,
  by = "memberid")
dataset$target <- factor(ifelse(dataset$target > 0, "Yes", "No"), levels = c("No", "Yes"))
dataset <- as.data.frame(dataset)
colnames(dataset) <- make.names(tolower(colnames(dataset)))
predictors <- setdiff(names(dataset), c("target","memberid"))
str(dataset)

mydata=dataset[,-1]
trainsamples <- createDataPartition(y = dataset$target, times=1, p = 0.75, list=FALSE)
trainsamples <- unlist(trainsamples)

m <- rpart(target ~ .,data=dataset,
           method="anova",
           control = rpart.settings,
           parms = list(split = "gini"))
plotcp(m)
summary(m)
printcp(m)
rpart.plot(m)
rpart.plot(prune(m, cp = 0.0075014))

#airbnb tree is_location_exact
airbnb$target=airbnb$room_type=="Private room"
m <- rpart(target ~ .,data=airbnb,
           method="class",
           control = rpart.settings,
           parms = list(split = "gini"))
plotcp(m)
summary(m)
printcp(m)
rpart.plot(m)
rpart.plot(prune(m, cp = 0.001))

#random forest on room_type, tune mtry
drivers <- c("property_type", "neighbourhood_cleansed", "room_type", 
             "accommodates", "bathrooms", "bedrooms", "beds", "bed_type", 
             "guests_included", "extra_people", "minimum_nights", "maximum_nights", 
             "host_response_time", "host_response_rate", "host_is_superhost", 
             "latitude", "longitude", "is_location_exact", "review_scores_rating", 
             "require_guest_phone_verification", "reviews_per_month")
mydata <- airbnb[, c("price", drivers)]
mydata <- na.exclude(mydata)
trainsamples <- createDataPartition(y = mydata$price, times=1, p = 0.75, list=FALSE)
trainsamples <- unlist(trainsamples)

## Fit the model
library(ranger)
set.seed(123456789)
m <- ranger(price ~ ., data = mydata[trainsamples, ],
            mtry = 3, num.trees = 500, importance = "impurity")
m 


