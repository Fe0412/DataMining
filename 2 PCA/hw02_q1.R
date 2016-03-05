setwd("/Users/jingyiyuan/Desktop/fall 2015/Data Mining/STAT homework/hw02")
faces_original = read.csv("hw02_q1_p1.csv",header = T)
dim(faces_original)
row = nrow(faces_original)
column = ncol(faces_original)
colmean_original = colMeans(faces_original, na.rm = FALSE, dims = 1)
rowmean_original = rowMeans(faces_original, na.rm = FALSE, dims = 1)

faces = array(NA,dim=c(row,column))
faces <- t(t(faces_original[,1:column])-colmean_original)

colmean = colMeans(faces)#to check if the data is centered
cov_matrix = (1/row) * t(faces) %*% faces

cov_eigen <- eigen(cov_matrix)
#cov_eigen$values are eigenvalues and  cov_eigen$vectors are eigenvectors

eigenvalues = cov_eigen$values
loading = cov_eigen$vectors#W
scores = faces %*% loading#Y

#trace = sum(eigenvalues)
num = vector()
proportn = vector()
for (i in 1:column){
  num[i] = i
  proportn[i] = sum(eigenvalues[1:i])/sum(eigenvalues)
}
plot(num,proportn)
#we get the first 3 values so that more than 99.5% of values can be explained

faces2_ori = read.csv("hw02_q1_p2.csv",header = T)
row2 = nrow(faces2_ori)
column2 = ncol(faces2_ori)
faces2 =  array(NA,dim=c(row2,column2))
faces2 <- t(t(faces2_ori[,1:column2])-colmean_original)

scores2 = as.matrix(faces2) %*% loading

k = 2
scores_first2 = scores2[,1:k]# Y 5*2
loading_2 = loading[,1:k]# W 5*2
proj_x = scores_first2 %*% t(loading_2)
projct_x = t(t(proj_x[,1:5]) + colmean_original)

distance = sum((proj_x - faces2)^2)
euclidean = distance^0.5

error = proj_x - faces2
