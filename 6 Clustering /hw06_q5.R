setwd("/Users/jingyiyuan/Desktop/Data Mining/R/")

a = c(0, 0.3, 0.4, 0.7, 0.3, 0, 0.5, 0.8, 0.4, 0.5, 0, 0.45, 0.7, 0.8, 0.45, 0)
matrix1 = as.dist(matrix(a, nrow = 4, ncol = 4))
dendrogram1 = hclust(matrix1, method = "complete")
plot(dendrogram1)

dendrogram2 = hclust(matrix1, method = "single")
plot(dendrogram2)

plot(dendrogram1, labels = c(2,1,4,3))
