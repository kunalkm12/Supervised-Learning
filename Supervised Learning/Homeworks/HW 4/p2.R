rm(list = ls())
library(ISLR)
library(MASS)
library(class)
library(ggplot2)
library(GGally)
library(caret)

### Loading data
data(Weekly)

### Summaries
summary(Weekly)
names(Weekly)
head(Weekly)
table(Weekly$Direction)

x11()
ggpairs(Weekly, columns = 1:8, aes(color=Direction))
x11()
plot(Weekly$Year, Weekly$Volume)

### Logistic regression
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = "binomial")
summary(fit)

### Prediction
pred <- predict(fit, type = "response")
pred <- round(pred)
dats <- as.data.frame(cbind(pred, as.numeric(Weekly[,9])-1))
wrong = 0
for(i in 1:nrow(Weekly))
{
  if(dats[i,1] != dats[i,2])
  {
    wrong = wrong + 1
  }
}
correct <- (nrow(Weekly)-wrong)/nrow(Weekly)
conf <- confusionMatrix(as.factor(pred), as.factor(as.numeric(Weekly[,9])-1))

### Division 2
subtrain <- subset(Weekly, Year <= 2008)
subtest <- subset(Weekly, Year > 2008)
subtrainy <- as.numeric(subtrain$Direction)-1
subtesty <- as.numeric(subtest$Direction)-1

### Logistic regression 2
fit <- glm(Direction ~ Lag2, data = subtrain, family = "binomial")
summary(fit)
subpred <- predict(fit, newdata = subtest, type = "response")
subpred <- round(subpred)

dats <- as.data.frame(cbind(subpred, subtesty))
wrong = 0
for(i in 1:nrow(subtest))
{
  if(dats[i,1] != dats[i,2])
  {
    wrong = wrong + 1
  }
}
accuracysub <- 1-(wrong/nrow(subtest))
subconf <- confusionMatrix(as.factor(subpred), as.factor(subtesty))

### Linear Discriminant Analysis
fit <- lda(Direction ~ Lag2, subtrain)
subpredlda <- predict(fit, subtest)
subpredlda <- as.numeric(subpredlda$class)-1
dats <- as.data.frame(cbind(subpredlda, subtesty))
wrong = 0
for(i in 1:nrow(subtest))
{
  if(dats[i,1] != dats[i,2])
  {
    wrong = wrong + 1
  }
}
accuracysublda <- 1-(wrong/nrow(subtest))
subconf <- confusionMatrix(as.factor(subpredlda), as.factor(subtesty))

### k-Nearest Neighbors with k = 1
subtrainx <- as.matrix(subtrain$Lag2)
subtestx <- as.matrix(subtest$Lag2)
fit <- knn(subtrainx, subtestx, subtrain$Direction, k = 1)
subpredknn <- as.numeric(fit)-1
dats <- as.data.frame(cbind(subpredknn, subtesty))
wrong = 0
for(i in 1:nrow(subtest))
{
  if(dats[i,1] != dats[i,2])
  {
    wrong = wrong + 1
  }
}
accuracysubknn <- 1-(wrong/nrow(subtest))
subconf <- confusionMatrix(as.factor(subpredknn), as.factor(subtesty))
