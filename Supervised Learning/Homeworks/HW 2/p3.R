rm(list = ls())
library(ISLR)
data(College)

### dividing into training and testing data
set.seed(123)
div <- sort(sample(nrow(College),nrow(College)*0.7))
train <- College[div,]
test <- College[-div,]
train[,1] <- as.numeric(train[,1])
test[,1] <- as.numeric(test[,1])

### Linear model
fit <- lm(Apps ~ ., data = train)

### Prediction
pred <- predict(fit, test)
error <- mean(summary(fit)$residuals^2)

### Ridge Regression Model
