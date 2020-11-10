rm(list = ls())
library(lattice)
library(MASS)
library(ISLR)

########## Exploratory Data Analysis

### Basic information about the Auto dataset
dim(Auto)
names(Auto)
head(Auto)

### Histograms
x11()
par(mfrow = c(1,4))
hist(Auto$mpg, breaks = 5, xlab = "Miles per Gallon", main = "Histogram: mpg")
hist(Auto$cylinders, breaks = 2.9 + (0:6)*1, xlab = "Cylinders", main = "Histogram: cylinders")
hist(Auto$displacement, xlab = "Displacement", main = "Histogram: displacement")
hist(Auto$horsepower, breaks = 40 + (0:10)*20, xlab = "Horsepower", main = "Histogram: horsepower")

x11()
par(mfrow = c(1,4))
hist(Auto$weight, xlab = "Weight", main = "Histogram: weight")
hist(Auto$acceleration, breaks = 8 + (0:9)*2, xlab = "Acceleration", main = "Histogram: acceleration")
hist(Auto$year, breaks = 12, xlab = "Year", main = "Histogram: year")
hist(Auto$origin, breaks = 0.9 + (0:3)*1, xlab = "Origin", main = "Histogram: origin")

### Density plots
x11()
dens <- density(Auto$mpg)
xl <- range(dens$x)
yl <- range(dens$y)
hist(Auto$mpg, breaks = 5, probability = T, xlim = xl, ylim = yl, xlab = "Miles per Gallon", main = "Density Plot: mpg")
lines(dens)

### Box plots
x11()
boxplot(Auto$mpg, horizontal = TRUE, main = "Boxplot: mpg")

x11()
boxplot(Auto$cylinders, horizontal = TRUE, main = "Boxplot: cylinders")

x11()
boxplot(Auto$displacement, horizontal = TRUE, main = "Boxplot: displacement")

x11()
boxplot(Auto$horsepower, horizontal = TRUE, main = "Boxplot: horsepower")

x11()
boxplot(Auto$weight, horizontal = TRUE, main = "Boxplot: weight")

x11()
boxplot(Auto$acceleration, horizontal = TRUE, main = "Boxplot: acceleration")

x11()
boxplot(Auto$year, horizontal = TRUE, main = "Boxplot: year")

x11()
boxplot(Auto$origin, horizontal = TRUE, main = "Boxplot: origin")

### Scatter Plots to compare effects of variables on mpg
x11()
par(mfrow = c(1,4))
plot(mpg ~ cylinders, data = Auto, main = "MPG ~ Cylinders", pch = 16)
plot(mpg ~ displacement, data = Auto, main = "MPG ~ Displacement", pch = 16)
plot(mpg ~ horsepower, data = Auto, main = "MPG ~ Horsepower", pch = 16)
plot(mpg ~ weight, data = Auto, main = "MPG ~ Weight", pch = 16)

x11()
par(mfrow = c(1,3))
plot(mpg ~ acceleration, data = Auto, main = "MPG ~ Acceleration", pch = 16)
plot(mpg ~ year, data = Auto, main = "MPG ~ Year", pch = 16)
plot(mpg ~ origin, data = Auto, main = "MPG ~ Origin", pch = 16)

### Smooth curves
x11()
par(mfrow = c(1,4))
plot(mpg ~ displacement, data = Auto, main = "MPG ~ Displacement", pch = 16)
with(Auto, lines(lowess(displacement, mpg)), lwd = 2)
plot(mpg ~ horsepower, data = Auto, main = "MPG ~ Horsepower", pch = 16)
with(Auto, lines(lowess(horsepower, mpg)), lwd = 2)
plot(mpg ~ weight, data = Auto, main = "MPG ~ Weight", pch = 16)
with(Auto, lines(lowess(weight, mpg)), lwd = 2)
plot(mpg ~ acceleration, data = Auto, main = "MPG ~ Acceleration", pch = 16)
with(Auto, lines(lowess(acceleration, mpg)), lwd = 2)

x11()
plot(mpg ~ year, data = Auto, main = "MPG ~ Year", pch = 16)
with(Auto, lines(lowess(year, mpg)), lwd = 2)

### Examining scatterplots broken down by multiple factors
x11()
xyplot(mpg ~ displacement | year, data = Auto, groups = origin, auto.key = list(columns=2))
x11()
xyplot(mpg ~ horsepower | year, data = Auto, groups = origin, auto.key = list(columns=2))
x11()
xyplot(mpg ~ weight | year, data = Auto, groups = origin, auto.key = list(columns=2))
x11()
xyplot(mpg ~ acceleration | year, data = Auto, groups = origin, auto.key = list(columns=2))


######### Cleaning
### Removing name variable
CleanAuto <- Auto[,1:8]
names(CleanAuto)

### Examining outliers
Out_horse <- subset(CleanAuto, horsepower>200)
Out_acceleration_low <- subset(CleanAuto, acceleration<10)
Out_acceleration_high <- subset(CleanAuto, acceleration>21)
save(CleanAuto, file = "CleanAuto.RData")