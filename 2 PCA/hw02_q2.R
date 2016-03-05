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
faces_matrix = matrix(data = NA, nrow = people_num*every_person, ncol = row*column)

for (i in 1:people_num){
  for (j in 1:every_person){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list[i] , dir_list[i] , view_list[j])
    face = read.pnm(file = filename)
    face_matrix = getChannels(face)
    faces_matrix[every_person*(i-1)+j,] = as.vector(face_matrix)
  }
}

#b
colmean = colMeans(faces_matrix)
dim(colmean) <- c(row,column)
mean_face = pixmapGrey(colmean)
plot(mean_face)
#save the mean face
filename = 'mean face.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#c
row_all = nrow(faces_matrix)
column_all = ncol(faces_matrix)
faces = array(NA,dim=c(row_all,column_all))
colmean = as.vector(colmean)
faces <- t(t(faces_matrix[,1:column_all]) - colmean)
check = colMeans(faces)#to check if the data is centered
PCA = prcomp(faces)
loading = PCA$rotation #32256*152, W
x = PCA$x #152*152
sdev = PCA$sdev #vector 152, the square roots of the eigenvalues
center = PCA$center#vector 32256
scale = PCA$scale#FALSE
eigenvalues = sdev^2

num = vector()
proportn = vector()
for (i in 1:152){
  num[i] = i
  proportn[i] = sum(eigenvalues[1:i])/sum(eigenvalues)
}
plot(num,proportn)

#d
par(mfrow=c(3,3))
#par(mar=c(1,1,1,1))
for (i in 1:9){
  y = loading[,i]
  dim(y) <- c(row,column)
  eigenface = pixmapGrey(y)
  plot(eigenface)
}

scores = faces %*% loading
#index = 20;
#proj_x = scores[20,] %*% t(loading)

#e
par(mfrow=c(5,5))
par(mar=c(1,1,1,1))
plot(mean_face)
rec_face = colmean
for(i in 1:24){
  y2 = scores[20,i] * t(loading[,i])
  rec_face = as.vector(rec_face) + y2
  dim(rec_face) <- c(row,column)
  rec_face_pic = pixmapGrey(rec_face)
  plot(rec_face_pic)
}

par(mfrow=c(5,5))
plot(mean_face)
rec_face_2 = colmean
i = 1
while (i<121){
  y3 = scores[20,i:(i+4)] %*% t(loading[,i:(i+4)])
  rec_face_2 = as.vector(rec_face_2) + y3
  dim(rec_face_2) <- c(row,column)
  rec_face_pic2 = pixmapGrey(rec_face_2)
  plot(rec_face_pic2)
  i = i + 5
}

#f
new_matrix = faces_matrix[5:152,]
new_colmean = colMeans(new_matrix)
new_row = nrow(new_matrix)
new_column = ncol(new_matrix)
new_faces = array(NA,dim=c(new_row,new_column))
new_colmean = as.vector(new_colmean)
new_faces <- t(t(new_matrix[,1:column_all]) - new_colmean)
new_check = colMeans(faces)#to check if the data is centered

#col = colMeans(new_faces)#new mean face
#face_f = new_faces - col

pca = prcomp(new_faces)
new_loading = pca$rotation #32256*148, W
#new_scores = new_faces %*% new_loading#Y
new_scores2 = faces %*% new_loading #152*148
proj_x = new_scores2[4,] %*% t(new_loading[,])

#proj_x = new_scores2 %*% t(new_loading)
fi = colSums(proj_x)
dim(fi) <- c(row,column)
fi_pic = pixmapGrey(fi)
plot(fi_pic)

