rm(list = ls())
library(MASS)
library(leaps)

### Reading Cereal data
dats <- read.csv("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 2/cereal.csv")
names(dats)
head(dats)

### Dividing into train and test data
set.seed(123)
div <- sort(sample(nrow(dats),nrow(dats)*0.7))
traindat <- dats[div,]
testdat <- dats[-div,]
train <- traindat[,-c(1:3)]
test <- testdat[,-c(1:3)]

### Fitting a linear model to data
fit <- lm(rating ~ ., data = train)

### Predicting
pred <- predict(fit, test)
summ <- summary(fit)
MSE <- mean(summ$residuals^2)
print(MSE)

### Forward subset selection
fwdsel <- regsubsets(rating ~ ., data = train, nvmax = 12, method = "forward")
sumfwd <- summary(fwdsel)
x11()
par(mfrow = c(2,2))
plot(sumfwd$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(sumfwd$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(sumfwd$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(sumfwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

### Exhaustive subset selection
exhsel <- regsubsets(rating ~ ., data = train, nvmax = 12, method = "exhaustive")
sumexh <- summary(exhsel)
x11()
par(mfrow = c(2,2))
plot(sumexh$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(sumexh$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(sumexh$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(sumexh$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")