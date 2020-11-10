rm(list = ls())
library(leaps)

set.seed(100)
p = 20
n = 1000
x = matrix(rnorm(n * p), n, p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
eps = rnorm(p)
y = x %*% B + eps

train = sample(seq(1000), 200, replace = FALSE)
xtrain = x[train, ]
xtest = x[-train, ]
ytrain = y[train, ]
ytest = y[-train, ]

fwd = regsubsets(y ~ ., data = data.frame(x = xtrain, y = ytrain), nvmax = p, method = "forward")
summfwd = summary(fwd)

### MSE calculation
errors = rep(0,p)
cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for(i in 1:p)
{
  coefi = coef(fwd, id = i)
  pred = as.matrix(xtrain[, cols %in% names(coefi)]) %*% coefi[names(coefi) %in% cols]
  errors[i] = mean((ytrain - pred)^2)
}
x11()
plot(errors, ylab = "MSE for Training data", xlab = "Model Size", pch = 19, type = "b")
which(errors == min(errors))

errors = rep(0,p)
for(i in 1:p)
{
  coefi = coef(fwd, id = i)
  pred = as.matrix(xtest[, cols %in% names(coefi)]) %*% coefi[names(coefi) %in% cols]
  errors[i] = mean((ytest - pred)^2)
}
x11()
plot(errors, ylab = "MSE for Testing data", xlab = "Model Size", pch = 19, type = "b")
bestest = which(errors == min(errors))

coefi = coef(fwd, id = bestest)