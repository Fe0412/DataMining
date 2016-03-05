setwd("/Users/jingyiyuan/Desktop/Data Mining/R")
library(pixmap)

#a
dir_list = dir(path="CroppedYale/",all.files=FALSE)
people_num = length(dir_list)
view_list = c('P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10' , 'P00A+010E+00')
every_person = length(view_list)

face_example = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
row = face_example@size[1]
column = face_example@size[2]
face_matrix_6a = matrix(data = NA, nrow = people_num*every_person, ncol = row*column)

for (i in 1:people_num){
  for (j in 1:every_person){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[i] , dir_list[i] , view_list[j])
    face = read.pnm(file = filename)
    face_matrix = getChannels(face)
    face_matrix_6a[every_person*(i-1)+j,] = as.vector(face_matrix)
  }
}
subject <- data.frame(subjectNumber = c(1:152), data = face_matrix_6a)

fm_6a_size = dim(face_matrix_6a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6a = floor(fm_6a_size[1]*4/5)
ntest_6a = fm_6a_size[1]-ntrain_6a
set.seed(1)
ind_train_6a = sample(1:fm_6a_size[1],ntrain_6a)#121
ind_test_6a = c(1:fm_6a_size[1])[-ind_train_6a]#31

index_test_6a = floor((ind_test_6a-1)/4)+1
index_train_6a = floor((ind_train_6a-1)/4)+1
picture_test_6a = (ind_test_6a-1) %% 4 + 1
picture_train_6a = (ind_train_6a-1) %% 4 + 1

#b
train = face_matrix_6a[ind_train_6a[1:121],]
test = face_matrix_6a[ind_test_6a[1:31],]
colmean = colMeans(train)

row_train = nrow(train)
column_train = ncol(train)
faces_train = array(NA,dim=c(row_train,column_train))
faces_train <- t(t(train[,1:column_train]) - colmean)
check = colMeans(faces_train)#to check if the data is centered
PCA = prcomp(faces_train)
loading = PCA$rotation #32256*121, W

scores_test = test %*% loading[,1:25]#31*25
scores_train = train %*% loading[,1:25]#121*25

t = vector()
index = vector()
right = 0
for (i in 1:31){
  dist_min = dist(rbind(scores_test[i,],scores_train[j,]))
  t[i] = 1
  for (j in 1:121){
    distance = dist(rbind(scores_test[i,],scores_train[j,]))
    if (distance < dist_min){
      dist_min = distance
      t[i] = j#the jth row in scores_train is closest to the ith row in scores_test
    }
  }
  index[i] =  ind_train_6a[t[i]]
  if(floor((index[i]-1)/4) == floor((ind_test_6a[i]-1)/4)){
    right = right + 1
  }
}
wrong =  31 - right

#c
view_list_c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')
face_matrix_6c = matrix(data = NA, nrow = people_num*every_person, ncol = row*column)

for (i in 1:people_num){
  for (j in 1:every_person){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[i] , dir_list[i] , view_list_c[j])
    face = read.pnm(file = filename)
    face_matrix = getChannels(face)
    face_matrix_6c[every_person*(i-1)+j,] = as.vector(face_matrix)
  }
}
subject_c <- data.frame(data = face_matrix_6c, subjectNumber = c(1:152))

fm_6c_size = dim(face_matrix_6c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6c = floor(fm_6c_size[1]*4/5)
ntest_6c = fm_6c_size[1]-ntrain_6c
set.seed(2)
ind_train_6c = sample(1:fm_6c_size[1],ntrain_6c)#121
ind_test_6c = c(1:fm_6c_size[1])[-ind_train_6c]#31

train_c = face_matrix_6c[ind_train_6c[1:121],]
test_c = face_matrix_6c[ind_test_6c[1:31],]
colmean_c = colMeans(train_c)

faces_train_c = array(NA,dim=c(row_train,column_train))
faces_train_c <- t(t(train_c[,1:column_train]) - colmean_c)
check = colMeans(faces_train_c)#to check if the data is centered
PCA_c = prcomp(faces_train_c)
loading_c = PCA_c$rotation #32256*31, W

scores_test_c = test_c %*% loading_c[,1:25]#31*25
scores_train_c = train_c %*% loading_c[,1:25]#121*25

t_c = vector()
index_c = vector()
right_c = 0
for (i in 1:31){
  dist_min_c = dist(rbind(scores_test_c[i,],scores_train_c[j,]))
  t_c[i] = 1
  for (j in 1:121){
    distance_c = dist(rbind(scores_test_c[i,],scores_train_c[j,]))
    if (distance_c < dist_min_c){
      dist_min_c = distance_c
      t_c[i] = j#the jth row in scores_train is closest to the ith row in scores_test
    }
  }
  index_c[i] = ind_train_6c[t_c[i]]
  if(floor((index_c[i]-1)/4) == floor((ind_test_6c[i]-1)/4)){
  right_c = right_c + 1
  }
}
wrong_c = 31 - right_c    
original_index = floor((ind_test_6c-1)/4) + 1
original_picture = (ind_test_6c-1) %% 4 + 1
classified_index = floor((index_c-1)/4) + 1
classified_picture = (index_c-1) %% 4 + 1

par(mfrow=c(1,2))
ori_file = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[original_index[1]] , dir_list[original_index[1]] , view_list_c[original_picture[1]])
ori_face = read.pnm(file = ori_file)
plot(ori_face, main = "original")
cla_file = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[classified_index[1]] , dir_list[classified_index[1]] , view_list_c[classified_picture[1]])
cla_face = read.pnm(file = cla_file)
plot(cla_face, main = "identified as")

#d
right_d = vector()
for (k in 1:10){
  set.seed(k+2)#seed from 3 to 12
  ind_train_6c = sample(1:fm_6c_size[1],ntrain_6c)#121
  ind_test_6c = c(1:fm_6c_size[1])[-ind_train_6c]#31

  train_c = face_matrix_6c[ind_train_6c[1:121],]
  test_c = face_matrix_6c[ind_test_6c[1:31],]
  colmean_c = colMeans(train_c)

  faces_train_c = array(NA,dim=c(row_train,column_train))
  faces_train_c <- t(t(train_c[,1:column_train]) - colmean_c)
  check = colMeans(faces_train_c)#to check if the data is centered
  PCA_c = prcomp(faces_train_c)
  loading_c = PCA_c$rotation #32256*31, W

  scores_test_c = test_c %*% loading_c[,1:25]#31*25
  scores_train_c = train_c %*% loading_c[,1:25]#121*25

  t_c = vector()
  index_c = vector()
  right_d[k] = 0
  for (i in 1:31){
    dist_min_c = dist(rbind(scores_test_c[i,],scores_train_c[j,]))
    t_c[i] = 1
    for (j in 1:121){
      distance_c = dist(rbind(scores_test_c[i,],scores_train_c[j,]))
      if (distance_c < dist_min_c){
        dist_min_c = distance_c
        t_c[i] = j#the jth row in scores_train is closest to the ith row in scores_test
      }
    }
    index_c[i] = ind_train_6c[t_c[i]]
    if(floor((index_c[i]-1)/4) == floor((ind_test_6c[i]-1)/4)){
      right_d[k] = right_d[k] + 1
    }
  }
}
all = rep(31,10)
wrong_d = all-right_d

#e
par(mfrow=c(2,4))
for (j in 1:every_person){
  filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[1] , dir_list[1] , view_list[j])
  person1 = read.pnm(file = filename)
  plot(person1)
}
for (j in 1:every_person){
  filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[1] , dir_list[1] , view_list_c[j])
  person1 = read.pnm(file = filename)
  plot(person1)
}

