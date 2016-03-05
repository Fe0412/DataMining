#problem 6
#(b)
#training and testing are the same as problem 5
#install.packages("glmnet")
library(glmnet)

train = as.data.frame(training[,1:4875])
x = scale(training[,1:4875], center = TRUE, scale = TRUE)
x[is.na(x)]<-0
y = training$y
train.ridge = cv.glmnet(x, y, alpha = 0, family = "binomial", type.measure = "class")

test = scale(testing[,1:4875], center = TRUE, scale = TRUE)
test[is.na(test)]<-0
y.pred.ridge = predict(train.ridge, test)
coefic = as.matrix(abs(coef(train.ridge)))

right_b = 0
false_n_b = 0
false_p_b = 0
h = sum(as.integer(testing$y)-1)
for (i in 1:length(y.pred.ridge)){
  if(i < h+1){
    if(y.pred.ridge[i] > 0.5){
      right_b = right_b + 1
    }
    else {false_n_b = false_n_b + 1}
  }
  else{
    if(y.pred.ridge[i] < 0.5){
      right_b = right_b + 1
    }
    else {false_p_b = false_p_b + 1}#Madison classified as Hamilton
  }
}
right_b = right_b/length(y.pred.ridge)
false_p_b = false_p_b/11
false_n_b = false_n_b/16

#(c)
train.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")
y.pred.lasso = predict(train.lasso, test)
coefic = as.matrix(abs(coef(train.lasso)))

right_c = 0
false_n_c = 0
false_p_c = 0
h = sum(as.integer(testing$y)-1)
for (i in 1:length(y.pred.lasso)){
  if(i < h+1){
    if(y.pred.lasso[i] > 0.5){
      right_c = right_c + 1
    }
    else {false_n_c = false_n_c + 1}
  }
  else{
    if(y.pred.lasso[i] < 0.5){
      right_c = right_c + 1
    }
    else {false_p_c = false_p_c + 1}#Madison classified as Hamilton
  }
}
right_c = right_c/length(y.pred.lasso)
false_p_c = false_p_c/11
false_n_c = false_n_c/16
