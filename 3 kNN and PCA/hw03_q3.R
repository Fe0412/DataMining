#a
setwd("/Users/jingyiyuan/Desktop/Data Mining/STAT homework/hw03")
data = read.csv("hw03_q3.csv",header = T)
row = nrow(data)
column = ncol(data)
distance = as.matrix(dist(data[,1:2]))
data2 =  sort(distance[1,],decreasing = F,index.return = T)

#b
y = vector()
kth_near = vector()
MSE_test = vector()
for (k in 1:10){
  index = data2$ix[k+1]
  kth_near[k] = data[index,3]
  y[k] = sum(kth_near)/k
  MSE_test[k] = (y[k]-data[1,3])^2
}

A = vector()
B = vector()
dataset_train = list(A,B)
index_train = vector()
y_train = vector()
kth_near_train = array(NA,dim=c(19,10))
MSE_train = vector()
for (k in 1:10){
  for (i in 1:19){
    dataset_train = sort(distance[2:20,i+1],decreasing = F,index.return = T)
    index_train[i] = (dataset_train$ix+1)[k]
    }
  kth_near_train[,k] = data[index_train,3]
  if(k==1){
    y_train = kth_near_train[,1]
  }
  if(k>1){
    y_train = rowMeans(kth_near_train[,1:k])#prediction of 19 y
  }
  MSE_train[k] = (sum((y_train-data[2:20,3])^2))/19 
}

#c
dataset_ctest = list(A,B)
index_ctest = vector()
yc = vector()
kth_nearc = vector()
MSE_testc = vector()
MSE_test_all = rep(0,10)
for (i in 1:20){
  for (k in 1:10){
    dataset_ctest = sort(distance[i,],decreasing = F,index.return = T)
    index_ctest = dataset_ctest$ix[k+1]
    kth_nearc[k] = data[index_ctest,3]
    yc[k] = mean(kth_nearc[1:k])
    MSE_testc[k] = (yc[k]-data[i,3])^2
    }
  MSE_test_all = MSE_test_all + MSE_testc
}
MSE_test_c = MSE_test_all/20

dataset_ctrain = list(A,B)
index_ctrain = vector()
y_ctrain = vector()
kth_near_ctrain = array(NA,dim=c(19,10))
MSE_train_all = rep(0,10)
MSE_ctrain = vector()
data_y = data[,3]
data_z = vector()
for (j in 2:20){
  for (k in 1:10){
    for (i in 1:19){
      if(j<20){
        distance_ctrain = rbind(distance[1:(j-1),],distance[(j+1):20,])
        distance_ctrain = cbind(distance_ctrain[,1:(j-1)],distance_ctrain[,(j+1):20])
      }
      if(j==20){
        distance_ctrain = distance[1:19,1:19]
      }
      dataset_ctrain = sort(distance_ctrain[i,],decreasing = F,index.return = T)
      if(dataset_ctrain$ix[k]>=j){
        index_ctrain[i] = (dataset_ctrain$ix+1)[k]
      }
      if(dataset_ctrain$ix[k]<j){
        index_ctrain[i] = (dataset_ctrain$ix)[k]  
      }
    }
    kth_near_ctrain[,k] = data[index_ctrain,3]
    if(k==1){
      y_ctrain = kth_near_ctrain[,1]
    }
    if(k>1){
      y_ctrain = rowMeans(kth_near_ctrain[,1:k])#prediction of 19 y
    }
    if(j<20){
      data_z = c(data_y[1:(j-1)],data_y[(j+1):20])
      MSE_ctrain[k] = (sum((y_ctrain-data_z)^2))/19
    }
    if(j==20){
      MSE_ctrain[k] = (sum((y_ctrain-data_y[1:19])^2))/19
    }
  }
  MSE_train_all = MSE_train_all + MSE_ctrain
}
MSE_train_c = (MSE_train_all + MSE_train)/20

#d
plot(1:10,MSE_train_c)
plot(1:10,MSE_test_c)
  