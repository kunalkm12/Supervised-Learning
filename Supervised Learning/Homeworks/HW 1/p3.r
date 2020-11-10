rm(list=ls())
library(MASS)
head(Boston)

### Pairwise Scatter Plots
x11()
pairs(Boston)

### Predictor associations with per capita crime rate
cor(Boston[-1], Boston$crim)

### High crime rates
x11()
hist(Boston$crim, breaks = 20)
mean(Boston$crim)
median(Boston$crim)
max(Boston$crim)
highcrime <- subset(Boston, crim>8)

### High tax rates
x11()
hist(Boston$tax, breaks = 20)
mean(Boston$tax)
median(Boston$tax)
max(Boston$tax)
hightax <- subset(Boston, tax>650)
hightax2 <- subset(Boston, tax>700)

### High pupil teacher ratios
x11()
hist(Boston$ptratio, breaks = 20)
mean(Boston$ptratio)
median(Boston$ptratio)
max(Boston$ptratio)
highptr <- subset(Boston, ptratio>21)

### More than 7 rooms
sevenrooms <- subset(Boston, rm>7)

### More than 8 rooms
eightrooms <- subset(Boston, rm>8)