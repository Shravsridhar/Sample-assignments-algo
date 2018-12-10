# Loading  dataset
data<-read.csv("Problem5data.csv")

# Splitting the data based on class labels
data.split = split(data, data$class)
Class1 = data.split$'1'
Class0 =data.split$'0'
Class0<-Class0[,-3]
Class1<-Class1[,-3]

# sample means for each class
mean_class1 = sapply(Filter(is.numeric, Class1), mean)
mean_class0 = sapply(Filter(is.numeric, Class0), mean)


cov_class1 = cov(Filter(is.numeric, Class1))
cov_class0 = cov(Filter(is.numeric, Class0))

# Between-class scatter matrix
B = (mean_class1 - mean_class0)%*%t(mean_class1 - mean_class0)

# Within-class scatter matrix

library(matlib)
S = cov_class1 + cov_class0
S

invSB = solve(S)%*%B

# Eigenvector and Eigen value
temp = eigen(invSB)
W = temp$vectors[,1]

# computing projected means
projmean_class1 = W%*%mean_class1
projmean_class0 = W%*%mean_class0
W0 = mean(c(projmean_class1,projmean_class0))

