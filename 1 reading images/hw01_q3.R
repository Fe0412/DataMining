#a
library (MASS)
Boston
?Boston
dim(Boston)
row = nrow(Boston)
col = ncol(Boston)

#b
pairs(Boston)

#d
attach(Boston)
range(crim)
range(tax)
range(ptratio)
hist(crim)
hist(tax)
hist(ptratio)
par(mfrow=c(1,3))
boxplot(crim)
boxplot(tax)
boxplot(ptratio)

#e
sum(chas)

#f
median(ptratio)

#g
min_medv = min(Boston$medv)
index = vector()
k = 1
for (i in 1:length(medv)){
  if (medv[i] == min_medv){
    index[k]=i
    k = k+1
  }
}
Boston[index,]

ranking = array(NA,dim=c(row,col))
for(i in 1:14){
  ranking[,i] = rank(Boston[,i])
}
ranking[index,]

#h
Boston[rm > 7,]
sum(rm > 7)
Boston_rooms_more = Boston[rm > 8,]
sum(rm > 8)
Boston_rooms_less = Boston[rm <= 8,]
par(mfrow=c(1,2))
boxplot(Boston_rooms_more)
boxplot(Boston_rooms_less)
