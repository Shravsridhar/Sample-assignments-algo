# Load data
cereal.df<-read.csv("Cereals.csv")
View(cereal.df)

# Identifying the class of each variable in the data frame
sapply(cereal.df,class)

# Nominal variable check
sapply(cereal.df, is.factor)

# Ordinal variable check
sapply(cereal.df, is.ordered)

str(cereal.df)

# Computing mean,median,min,max,standard deviation
# Taking into account only the quantitative variables
library(dplyr)
num.cereal.df<-select_if(cereal.df,is.numeric)
sapply(num.cereal.df,mean,na.rm=TRUE)
sapply(num.cereal.df,median,na.rm=TRUE)
sapply(num.cereal.df,min,na.rm=TRUE)
sapply(num.cereal.df,max,na.rm=TRUE)
sapply(num.cereal.df,sd,na.rm=TRUE)

# Summary statistics of dataset
summary(cereal.df)

# Histogram for each variable
par(mfrow=c(1,1))
attach(num.cereal.df)
hist(calories)
hist(protein)
hist(carbo)
hist(cups)
hist(fat)
hist(fiber)
hist(potass)
hist(rating)
hist(shelf)
hist(sodium)
hist(sugars)
hist(vitamins)
hist(weight)

summary(cereal.df)

# Side by side box plot
par(mfrow=c(1,1))
attach(cereal.df)
boxplot(calories~ type, main="Calories by cereal type",xlab="Cereal Type", ylab="Calories")
boxplot(rating ~ shelf, main="Consumer rating", ylab ="Consumer rating",xlab="shelf height")

# Correlation table
library(e1071)
library(VIM)
cor(num.cereal.df) # weight calories highly correlated
matrixplot(num.cereal.df,delimiter=NULL,sortby=NULL)

# Normalize
library(normalr)
normalise(num.cereal.df)



