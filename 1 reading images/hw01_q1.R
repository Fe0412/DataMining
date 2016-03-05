setwd("/Users/jingyiyuan/Desktop/Data Mining/R")
college = read.csv("College.csv",header = T)

rownames(college) = college[,1]
fix(college)

college = college[,-1]
fix(college)

summary(college)

A <- college[,1:10]
pairs(A)

attach(college)
plot(Private, Outstate, xlab = "Private", ylab = "Outstate")

Elite = rep ("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes"
Elite = as.factor (Elite)
college = data.frame(college ,Elite)
summary(Elite)
plot(Elite, Outstate, xlab = "Elite", ylab = "Outstate")

par(mfrow=c(2,2))
hist(Top10perc,col = 1, breaks = 10)
hist(Apps,col = 2, breaks = 20)
hist(Accept,col = 3, breaks = 40)
hist(Enroll,col = 4, breaks = 80)

#continue explore more
admission_rate = Accept/Apps
range(admission_rate)

