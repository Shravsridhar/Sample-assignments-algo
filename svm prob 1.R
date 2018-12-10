library(e1071)
library(forecast)
library(ModelMetrics)

# Loading the data 
bank.df<-read.csv("Universalbank_1500.csv")
# Removing the ID and Zip code predictors
bank.df<-bank.df[ , -c(1, 5)]
bank.df$Personal.Loan<-as.factor(bank.df$Personal.Loan)


# cross validation
set.seed(1)
index = sample(2, nrow(bank.df), replace=TRUE, prob=c(0.7, 0.3))
train.df = bank.df[index == 1, ]
test.df = bank.df[index ==2, ]

# Training an SVM model
svm.model = svm(Personal.Loan~ ., data = train.df, cost = 100, gamma = 1, 
            type = "C-classification", kernel = "linear")
# Predicting 
svm.pred = predict(svm.model, test.df[,-8])

# Support vectors
summary(svm.model)

# The number of support vectors in total are 128
head(svm.model$SV)

# Weights of support vectors
supportvector = cbind(svm.model$index, svm.model$coefs)
head(supportvector)
# w = sum x_i alpha_i 
X = as.matrix(train.df[,-8]) 
w = t(svm.model$coefs) %*% X[svm.model$index,-8] 
print(w)

# Training Accuracy 

svm.train = predict(svm.model, train.df[,-8])
table(pred = svm.train, Actual = train.df[,8])

# Testing Accuracy
svm.test = predict(svm.model, test.df[,-8])
table(pred = svm.test, Actual = test.df[,8])


