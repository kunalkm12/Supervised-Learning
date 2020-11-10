rm(list = ls())
library(lattice)
library(MASS)

### Loading cleaned data
load("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining I/Homeworks/HW 1/CleanAuto.Rdata")

### Multiple Regression
reg <- lm(mpg ~ ., data = CleanAuto)
sall <- summary(reg) 

reg <- lm(mpg ~ cylinders, data = CleanAuto)
scyl <- summary(reg)

reg <- lm(mpg ~ displacement, data = CleanAuto)
sdis <- summary(reg)

reg <- lm(mpg ~ horsepower, data = CleanAuto)
shor <- summary(reg)

reg <- lm(mpg ~ weight, data = CleanAuto)
swei <- summary(reg)

reg <- lm(mpg ~ acceleration, data = CleanAuto)
sacc <- summary(reg)

reg <- lm(mpg ~ year, data = CleanAuto)
syea <- summary(reg)

reg <- lm(mpg ~ origin, data = CleanAuto)
sori <- summary(reg)

