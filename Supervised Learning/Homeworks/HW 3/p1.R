rm(list = ls())
library(leaps)
library(glmnet)

### Reading the train and test data
train <- read.delim("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 3/ticdata2000.txt", header = FALSE, sep = '\t')
test <- read.delim("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 3/ticeval2000.txt", header = FALSE, sep = '\t')
ytrue <- read.delim("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 3/tictgts2000.txt", header = FALSE, sep = '\t')

### Explore train and test data
head(train)
names(train)
head(test)
names(test)
table(train[,86])
table(ytrue)

### OLS estimates
fit <- lm(V86 ~ ., data = train)
summary(fit)
pred <- predict(fit, test)
summary(pred)
predtrain <- predict(fit, train)
summary(predtrain)

### Calculating accuracy of predictions
pred <- as.data.frame(pred)
predtrain <- as.data.frame(predtrain)
comptest <- cbind(pred, ytrue)
comptrain <- cbind(predtrain, train[,86])
comptest <- as.matrix(comptest)
comptrain <- as.matrix(comptrain)

wrong <- 0
for(i in 1:nrow(comptest))
{
  if(round(comptest[i,1]) != comptest[i,2])
  {
    wrong = wrong + 1
  }
}
error <- (wrong/nrow(comptest))*100
accuracyOLStest <- 100-error

wrong <- 0
for(i in 1:nrow(comptrain))
{
  if(round(comptrain[i,1]) != comptrain[i,2])
  {
    wrong = wrong + 1
  }
}
error <- (wrong/nrow(comptrain))*100
accuracyOLStrain <- 100-error
OLSones <- table(round(comptest[,1]))



### Forwards selection
fwd <- regsubsets(V86 ~ ., train, nvmax = 85, method = "forward")
fwdsumm <- summary(fwd)
fwdsumm$outmat

x11()
par(mfrow = c(2,2))
plot(fwdsumm$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(fwdsumm$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(fwdsumm$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(fwdsumm$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

which(fwdsumm$cp == min(fwdsumm$cp))
which(fwdsumm$bic == min(fwdsumm$bic))

store_error_train <- c()
store_error_test <- c()

for(i in 1:85)
{
  coefi = coef(fwd, id = i)
  coefi = coefi[-1]
  predtest = as.matrix(test[, names(coefi)])%*%coefi
  predtrain = as.matrix(train[, names(coefi)])%*%coefi
  
  errtest = (1/length(as.matrix(ytrue)))*sum((ytrue-predtest)^2)
  errtrain = (1/length(train[,86]))*sum((train[,86]-predtrain)^2)
  
  store_error_train = c(store_error_train, errtrain)
  store_error_test = c(store_error_test, errtest)
}

which(store_error_test == min(store_error_test))
errtest = store_error_test[9]
errtrain = store_error_train[9]
accuracyfwdtest = (1-errtest)*100
accuracyfwdtrain = (1-errtrain)*100

coefi = coef(fwd, id = 9)[-1]
predtest = as.matrix(test[, names(coefi)])%*%coefi
predtrain = as.matrix(train[, names(coefi)])%*%coefi



### Backwards selection
bwd <- regsubsets(V86 ~ ., train, nvmax = 85, method = "backward")
bwdsumm <- summary(bwd)

x11()
par(mfrow = c(2,2))
plot(bwdsumm$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(bwdsumm$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(bwdsumm$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(bwdsumm$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

which(bwdsumm$cp == min(bwdsumm$cp))
which(bwdsumm$bic == min(bwdsumm$bic))

store_error_train <- c()
store_error_test <- c()

for(i in 1:85)
{
  coefi = coef(bwd, id = i)
  coefi = coefi[-1]
  predtest = as.matrix(test[, names(coefi)])%*%coefi
  predtrain = as.matrix(train[, names(coefi)])%*%coefi
  
  errtest = (1/length(as.matrix(ytrue)))*sum((ytrue-predtest)^2)
  errtrain = (1/length(train[,86]))*sum((train[,86]-predtrain)^2)
  
  store_error_train = c(store_error_train, errtrain)
  store_error_test = c(store_error_test, errtest)
}

which(store_error_test == min(store_error_test))
errtest = store_error_test[23]
errtrain = store_error_train[23]
accuracybwdtest = (1-errtest)*100
accuracybwdtrain = (1-errtrain)*100

coefi = coef(bwd, id = 23)[-1]
predtest = as.matrix(test[, names(coefi)])%*%coefi
predtrain = as.matrix(train[, names(coefi)])%*%coefi



### LASSO 
lasso <- glmnet(as.matrix(train[,-86]), train[,86], alpha = 1)
x11()
plot(lasso)

cv.out <- cv.glmnet(as.matrix(train[,-86]), train[,86], alpha = 1)
x11()
plot(cv.out)
lambest <- cv.out$lambda.min

pred <- predict(lasso, s = lambest, type = "coefficients")
pred2 <- predict(lasso, s = lambest, newx = as.matrix(train[,-86]), type = "response")
pred2 <- round(pred2)
table(pred2)
comp <- cbind(pred2, train[,86])
wrong <- 0
for(i in 1:nrow(comp))
{
  if(comp[i,1] != comp[i,2])
  {
    wrong = wrong + 1
  }
}
err <- (wrong/nrow(comp))*100
accuracylassotrain <- 100 - err

pred2 <- predict(lasso, s = lambest, newx = as.matrix(test), type = "response")
pred2 <- round(pred2)
table(pred2)
comp <- cbind(pred2, ytrue)
wrong <- 0
for(i in 1:nrow(comp))
{
  if(comp[i,1] != comp[i,2])
  {
    wrong = wrong + 1
  }
}
err <- (wrong/nrow(comp))*100
accuracylassotest <- 100 - err



### Ridge Regression
ridge <- glmnet(as.matrix(train[,-86]), train[,86], alpha = 0)

cv.out <- cv.glmnet(as.matrix(train[,-86]), train[,86], alpha = 0)
x11()
plot(cv.out)
lambest <- cv.out$lambda.min

pred2 <- predict(ridge, s = lambest, newx = as.matrix(train[,-86]), type = "response")
pred2 <- round(pred2)
table(pred2)
comp <- cbind(pred2, train[,86])
wrong <- 0
for(i in 1:nrow(comp))
{
  if(comp[i,1] != comp[i,2])
  {
    wrong = wrong + 1
  }
}
err <- (wrong/nrow(comp))*100
accuracyridgetrain <- 100 - err

pred2 <- predict(ridge, s = lambest, newx = as.matrix(test), type = "response")
pred2 <- round(pred2)
table(pred2)
comp <- cbind(pred2, ytrue)
wrong <- 0
for(i in 1:nrow(comp))
{
  if(comp[i,1] != comp[i,2])
  {
    wrong = wrong + 1
  }
}
err <- (wrong/nrow(comp))*100
accuracyridgetest <- 100 - err
