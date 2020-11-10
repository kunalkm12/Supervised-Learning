rm(list = ls())
library(class)
library(caret)
library(ggfortify)
library(pls)

### Loading data
data(iris)
head(iris)
summary(iris)

### Division into training and test
set.seed(1234)
div <- sample(nrow(iris), nrow(iris)*0.7)
train <- iris[div, ]
test <- iris[-div, ]
trainy <- train[, 5]
testy <- test[, 5]
training <- train[, -5]
testing <- test[, -5]

### KNN on different values of k
k_vals = c(1,3,5,7,9,11,13,15)
store_error_train <- c()
store_error_test <- c()

for (i in 1:8){
  
  kk <- k_vals[i]
  
  testknn <- knn(training, testing, trainy, k = kk)
  trainknn <- knn(training, training, trainy, k = kk)
  
  testwrong <- length(which(testknn != testy))
  trainwrong <- length(which(trainknn != trainy))
  
  errtest <- testwrong/nrow(testing)
  errtrain <- trainwrong/nrow(training)
  
  store_error_test <- c(store_error_test, errtest)
  store_error_train <- c(store_error_train, errtrain)
}

which(store_error_test == min(store_error_test))

x11()
plot(k_vals, store_error_test, type = "b", xlab = "k values", ylab = "Testing Error")

x11()
plot(k_vals, store_error_train, type = "b", xlab = "k values", ylab = "Training Error")

### Confusion Matrix 
bestknn <- knn(training, testing, trainy, k = 7)
conf <- confusionMatrix(bestknn, testy)



### Principal components
prc <- prcomp(iris[,-5], scale = TRUE)
pc1 <- prc$x[,1]
pc2 <- prc$x[,2]
dats <- cbind(pc1,pc2)
dats <- cbind(dats, iris[,5])

set.seed(100)
div <- sample(nrow(iris), nrow(iris)*0.7)
training <- dats[div,]
testing <- dats[-div,]

store_error_train <- c()
store_error_test <- c()

for (j in 1:8) 
{
  kk = k_vals[i]
  
  testknn <- knn(training[,-3], testing[,-3], training[,3], k = kk)
  trainknn <- knn(training[,-3], training[,-3], training[,3], k = kk)
  
  testwrong <- length(which(testknn != testing[,3]))
  trainwrong <- length(which(trainknn != training[,3]))
  
  errtest <- testwrong/nrow(testing)
  errtrain <- trainwrong/nrow(training)
  
  store_error_test <- c(store_error_test, errtest)
  store_error_train <- c(store_error_train, errtrain)
}
x11()
plot(k_vals, store_error_test, type = "b", xlab = "k values", ylab = "Testing Error")

x11()
plot(k_vals, store_error_train, type = "b", xlab = "k values", ylab = "Training Error")

### Confusion Matrix 
bestknn <- knn(training[,-3], testing[,-3], training[,3], k = 7)
conf <- confusionMatrix(bestknn, as.factor(testing[,3]))

x11()
autoplot(prc, data = iris, colour = 'Species')