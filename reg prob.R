# Part b
# Loading the dataset
housing.df<- read.csv("BostonHousing.csv")

# Selecting the required predictore variables
predictors <- c(1,4,6,13)

# Training the dataset
train.index <- sample(c(1:506),350)
train.df <- housing.df[train.index,predictors] 
valid.df <- housing.df[-train.index,predictors]

# Fitting a multiple regression model
multi.lm <- lm(MEDV~., data = train.df)
options(scipen=999)

# Summary of the fitted model
summary(multi.lm)

print(multi.lm)
# Equation : y = -26.5360 - 0.2528x(CRIM) + 4.5970x(CHAS) + 7.9441x(RM)

# part c

# Predicting a new tract with CRIM = 0.1, CHAS = 0, RM = 6
library(forecast)
testdata.df <- data.frame(CRIM=0.1,CHAS=0,RM=6)
newtract.df <- predict(multi.lm,testdata.df)
options(scipen=999)
data.frame("Prediction_for_new_tract" = newtract.df)

# Histogram of Prediction error
residual<-train.df$MEDV - newtract.df
hist(residual,breaks=5,xlab = "residuals",main = "Prediction Error")
accuracy(newtract.df,mean(train.df$MEDV))

# Part d
# Finding relatonship between features INDUS, NOX, TAX

multi1.lm <- lm(MEDV~.,data=housing.df)
summary(multi1.lm)
 # all three of them seem to have positive relationship

#  Computing correlation table to find correlations amongst the predictor variable
housing1.df <- housing.df[,1:12]
x<-cor(housing1.df)

# to check for multicollinearity
library(car)
vif(multi1.lm)

# To check
findCorrelation(x)



