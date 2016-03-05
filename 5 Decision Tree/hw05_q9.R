#setwd("/Users/jingyiyuan/Desktop/Data Mining/R/")
#install.packages("ISLR")
library(ISLR)
data("Hitters")

#(a)
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

#(b)
row_H = nrow(Hitters)
train_set = Hitters[1:200,]
test_set = Hitters[201:nrow(Hitters),]

#(c)
p =  seq(-10, -0.1, by = 0.1)
lambda = 10 ^ p
MSE = vector()
#install.packages("gbm")
library(gbm)
for (i in 1:length(lambda)) {
  boosting = gbm(Salary ~ ., data = train_set, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  pred_y = predict(boosting, train_set, n.trees = 1000)
  MSE[i] = mean((train_set$Salary - pred_y)^2)
}
plot(lambda, MSE, xlab = "Shrinkage values", ylab = "Training set MSE")

#(d)
set.seed(1)
MSE_test = vector()
for (i in 1:length(lambda)) {
  boosting_test = gbm(Salary ~ ., data = train_set, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  pred_y_test = predict(boosting_test, test_set, n.trees = 1000)
  MSE_test[i] = mean((test_set$Salary - pred_y_test)^2)
}
plot(lambda, MSE_test, xlab = "Shrinkage values", ylab = "Testing set MSE")

#(e)
#linear regression
fit_linear = lm(Salary ~ ., data = train_set)
pred_linear = predict(fit_linear, test_set)
MSE_liner = mean((pred_linear - test_set$Salary)^2)
#lda
library(MASS)
fit_lda = lda(Salary ~ ., data = train_set)
pred_lda = predict(fit_lda, test_set)$class
pred_lda = as.numeric(as.character(pred_lda))
MSE_lda = mean((pred_lda - test_set$Salary)^2)

#(f)
for (i in 1:length(MSE_test)) {
  if(MSE_test[i] == min(MSE_test))
    k = lambda[i]
}
library(gbm)
boosting_f = gbm(Salary ~ ., data = train_set, distribution = "gaussian", n.trees = 1000, shrinkage = k)
summary(boosting_f)

#(g)
#install.packages("randomForest")
library(randomForest)
bagging = randomForest(Salary ~ ., data = train_set, ntree = 1000)
pred_y_bag = predict(bagging, newdata = test_set)
MSE_bagging = mean((pred_y_bag - test_set$Salary)^2)


