rm(list = ls())
library(class)
library(ggplot2)

### Loading data
load("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 2/zip.train.RData")
load("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 2/zip.test.RData")

### Focusing on 2s and 3s
training <- as.data.frame(zip.train[zip.train[,1] %in% c(2,3),])
testing <- as.data.frame(zip.test[zip.test[,1] %in% c(2,3),])
k_vals = c(1,3,5,7,9,11,13,15)

### Linear regression
fit <- lm(training$V1 ~ ., data = training[,-1])

### Prediction
pred <- predict(fit, testing[,-1])
arpred <- as.array(pred)
for(i in 1:364)
{
  if(arpred[i]>2.5)
  {
    arpred[i] = 3
  }
  else
  {
    arpred[i] = 2
  }
}
error = length(which(arpred!=testing[,1]))/364

### knn classification
store_error <- c()
store_error2 <- c()
for (i in 1:8){
  require(class)
  # select "k"
  kk <- k_vals[i]
  
  # apply the algorithm
  fitknn <- knn(training, testing, training[,1], k = kk)
  fitknntr <- knn(training, training, training[,1], k = kk)
  # calculate the error
  wrong <- length(which(fitknn != testing[,1]))
  wrong2 <- length(which(fitknntr != training[,1]))
  err <- wrong/364
  err2 <- wrong2/1389
  store_error <- c(store_error, err)
  store_error2 <- c(store_error2, err2)
}