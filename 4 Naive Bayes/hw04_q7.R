setwd("/Users/jingyiyuan/Desktop/Data Mining/R")
Auto = read.csv("Auto.csv", na.string = "?", header = T)
Auto <- na.omit(Auto)
row = nrow(Auto)#392
col = ncol(Auto)#9
attach(Auto)

#(a)
mpg01 = vector()
med = median(Auto[,1])
for (i in 1:row){
  if(Auto[i,1]>med) {mpg01[i] = 1}
  else {mpg01[i] = 0}
}
row = nrow(Auto)
data_auto <- data.frame(mpg01, Auto[1:row,2:col] )

#(b)
pairs(data_auto)
par(mfrow=c(2,3))
boxplot(cylinders~mpg01, data = data_auto, main = "y = cylinders,x = mpg01")
boxplot(displacement~mpg01, data = data_auto, main = "y = displacement,x = mpg01")#
boxplot(horsepower~mpg01, data = data_auto, main = "y = horsepower,x = mpg01")#
boxplot(weight~mpg01, data = data_auto, main = "y = weight,x = mpg01")#
boxplot(acceleration~mpg01, data = data_auto, main = "y = acceleration,x = mpg01")#
boxplot(year~mpg01, data = data_auto, main = "y = year,x = mpg01")

#(c)
train_num = (year%%2 == 0)  # if the year is even
test_num = !train_num
train = data_auto[train_num, ]
test = data_auto[test_num, ]
mpg01_test = mpg01[test_num]

#(d) LDA
#install.packages("lda")
library(MASS)
lda_a = lda(mpg01~cylinders + displacement + horsepower + weight, data = train)
pred.test <- predict(lda_a,test)$class

# Test error
table(pred.test, mpg01_test)
test_error_lda = 1 - mean(pred.test == mpg01_test)

#(e) QDA
qda_a = qda(mpg01~cylinders + displacement + horsepower + weight, data = train)
pred.test_qda <- predict(qda_a,test)$class

# Test error
table(pred.test_qda, mpg01_test)
test_error_qda = 1 - mean(pred.test_qda == mpg01_test)

#(f) Logistic Regression
LR_a = glm(mpg01~cylinders + displacement + horsepower + weight, data = train, family = "binomial")
LR_a$coefficients
pred.test_LR <- predict(LR_a, test, type = "response")

for (i in 1:length(pred.test_LR)){
  if(pred.test_LR[i] > 0.5) {pred.test_LR[i] = 1}
  else {pred.test_LR[i] = 0}
}
# Test error
table(pred.test_LR, mpg01_test)
test_error_LR = 1 - mean(pred.test_LR == mpg01_test)

#(g) kNN
related_variables = data_auto[,2:5]
train_kNN = related_variables[train_num, ]
test_kNN = related_variables[test_num, ]
mpg01_train = mpg01[train_num]
library(class)
k = c(1,5,10,50,100)
pred.test_kNN = matrix(data = NA, nrow = length(k), ncol = length(mpg01_test))
one = matrix(data = 1, nrow = 1, ncol = length(mpg01_test))
test_error_kNN = vector()
for (i in 1:length(k)){
  pred.test_kNN[i,] = knn(train_kNN, test_kNN, mpg01_train, k = k[i])#, prob=TRUE)
  pred.test_kNN[i,] =  as.numeric(as.character(pred.test_kNN[i,])) - one
  table(pred.test_kNN[i,], mpg01_test)
  test_error_kNN[i] = 1 - mean(pred.test_kNN[i,] == mpg01_test)
}
#1NN
#pred.test_1NN = knn(train_kNN, test_kNN, mpg01_train, k = 1, prob=TRUE)
#table(pred.test_1NN, mpg01_test)
#test_error_1NN = 1 - mean(pred.test_1NN == mpg01_test)
#5NN
#pred.test_5NN = knn(train_kNN, test_kNN, mpg01_train, k = 5, prob=TRUE)
#table(pred.test_5NN, mpg01_test)
#test_error_5NN = 1 - mean(pred.test_5NN == mpg01_test)
#10NN
#pred.test_10NN = knn(train_kNN, test_kNN, mpg01_train, k = 10, prob=TRUE)
#table(pred.test_10NN, mpg01_test)
#test_error_10NN = 1 - mean(pred.test_10NN == mpg01_test)
#50NN
#pred.test_50NN = knn(train_kNN, test_kNN, mpg01_train, k = 50, prob=TRUE)
#table(pred.test_50NN, mpg01_test)
#test_error_50NN = 1 - mean(pred.test_50NN == mpg01_test)
#100NN
#pred.test_100NN = knn(train_kNN, test_kNN, mpg01_train, k = 100, prob=TRUE)
#table(pred.test_100NN, mpg01_test)
#test_error_100NN = 1 - mean(pred.test_100NN == mpg01_test)

