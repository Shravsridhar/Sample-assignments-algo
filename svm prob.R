library(e1071)

# Loading the data
data.df<-read.csv("Problem4.csv")
df<-data.frame(data.df)
df$Class<- factor(df$Class)

# Running an SVM Model
svm.mod<-svm(Class~.,data = data.df,kernel="linear",gamma=1, type = "C-classification", scale = FALSE)

# Support Vectors
summary(svm.mod)
# There are 4 support vectors

print(svm.mod$SV)

# Coefficients
svm.mod$coefs

# Weight 
supportvector = cbind(svm.mod$index, svm.mod$coefs)
head(supportvector)
# w = sum x_i alpha_i 
X = as.matrix(data.df[,-3]) 
w = t(svm.mod$coefs) %*% X[svm.mod$index,] 
print(w)

# bias


x2 = matrix(c(2,4),nrow = 2,ncol = 1)
b2 = (w) %*% x2
x6 = matrix(c(2.1,2.5),nrow = 2,ncol = 1)
b6 = (w) %*% x6
x8 = matrix(c(9.1,4.5),nrow = 2,ncol = 1) 
b8 = (w) %*% x8

# slack
x1 = matrix(c(3.5,4),nrow = 2,ncol = 1)
b1 = (w) %*% x1






