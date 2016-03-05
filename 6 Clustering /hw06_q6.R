setwd("/Users/jingyiyuan/Desktop/Data Mining/R/")

#(a)
X1 = c(1, 1, 0, 5, 6, 4)
X2 = c(4, 3, 4, 1, 2, 0)
plot(X2 ~ X1)

#(b)
K = 2
n = 6
set.seed(1)
labels = sample(K, n, replace = T)
plot(X1, X2, col = labels)

#(c)
mu1_x1 = mean(X1[labels == 1])
mu1_x2 = mean(X2[labels == 1])
mu2_x1 = mean(X1[labels == 2])
mu2_x2 = mean(X2[labels == 2])
centroid1 = c(mu1_x1, mu1_x2)
centroid2 = c(mu2_x1, mu2_x2)
points(centroid1[1], centroid1[2], col = 1, pch = 24)
points(centroid2[1], centroid2[2], col = 2, pch = 24)

#(d)
labels_d = c(1, 1, 1, 2, 2, 2)
plot(X1, X2, col = labels_d)

#(e)
mu1_x1 = mean(X1[labels_d == 1])
mu1_x2 = mean(X2[labels_d == 1])
mu2_x1 = mean(X1[labels_d == 2])
mu2_x2 = mean(X2[labels_d == 2])
centroid1 = c(mu1_x1, mu1_x2)
centroid2 = c(mu2_x1, mu2_x2)
points(centroid1[1], centroid1[2], col = 1, pch = 24)
points(centroid2[1], centroid2[2], col = 2, pch = 24)

#(f)
plot(X1, X2, col = labels_d)

