# feature selection with lasso / elastic net (regularization / shrinkage)

library(glmnet)
library(ISLR)

x = model.matrix(Salary ~ . - 1, data = Hitters)
y = Hitters$Salary
fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
x = model.matrix(Salary ~ . - 1, data = Hitters)
y = Hitters$Salary
fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
y=na.omit(y)
fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)
fit = glmnet(x, y)
fit
plot(fit)
cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
