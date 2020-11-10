rm(list = ls())
library(klaR)
library(class)
library(ggplot2)
library(GGally)

### Loading data
load("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 4/Diabetes.RData")
names(Diabetes)
head(Diabetes)
table(Diabetes$group)

### Pairwise scatterplot
x11()
ggpairs(Diabetes, columns = 1:5, aes(color=group))

### Division into training and testing data
set.seed(1000)
div <- sample(nrow(Diabetes), nrow(Diabetes)*0.7)
train <- Diabetes[div, ]
test <- Diabetes[-div, ]
trainy <- Diabetes[div, 6]
testy <- Diabetes[-div, 6]
table(trainy)
table(testy)

### Linear Discriminant Analysis
ldafit <- lda(group ~ ., data = train)
predtest <- predict(ldafit, test[,-6])
predtrain <- predict(ldafit, train[,-6])

wrong = 0
for(i in 1:nrow(test))
{
  if(predtest$class[i] != testy[i])
  {
    wrong = wrong + 1
  }
}
accuracyLDAtest <- 1 - (wrong/nrow(test))

wrong = 0
for(i in 1:nrow(train))
{
  if(predtrain$class[i] != trainy[i])
  {
    wrong = wrong + 1
  }
}
accuracyLDAtrain <- 1 - (wrong/nrow(train))

### Quadratic Discriminant Analysis
qdafit <- qda(group ~ ., data = train)
predtest <- predict(qdafit, test[,-6])
predtrain <- predict(qdafit, train[,-6])

wrong = 0
for(i in 1:nrow(test))
{
  if(predtest$class[i] != testy[i])
  {
    wrong = wrong + 1
  }
}
accuracyQDAtest <- 1 - (wrong/nrow(test))

wrong = 0
for(i in 1:nrow(train))
{
  if(predtrain$class[i] != trainy[i])
  {
    wrong = wrong + 1
  }
}
accuracyQDAtrain <- 1 - (wrong/nrow(train))

### individual case
dats <- data.frame(1.86,184,0.98,122,544)
cols <- names(Diabetes[,-6])
colnames(dats) <- cols
predlda <- predict(ldafit, dats)
predqda <- predict(qdafit, dats)
predlda$class
predqda$class