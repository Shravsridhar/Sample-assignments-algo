# Load data
housing.df<-read.csv("BostonHousing.csv")
View(housing.df)
library(caret)

# Preprocess at same scale- standardize :combine center and scale
preProcessparameter<-preProcess(housing.df,method = c("center","scale"))
print(preProcessparameter)
transformed<-predict(preProcessparameter,housing.df)
summary(transformed) 

#Correlation analysis 
cor_coef<-cor(housing.df[-13],MEDV>30)
cor_coef

# Rank variables
rank_order<-order(-cor_coef)
rank_order
head(rank_order,5)

# PCA
pc_original<-prcomp(housing.df)
pc_transformed<-prcomp(transformed,scale.=T)
pc_original
pc_original$rotation[,1:2]
pc_transformed$rotation[,1:2]
library(ggfortify)
autoplot(pc_original,pc_transformed)

#no. of PC's contributing more than 90% for both cases
summary(pc_original)
summary(pc_transformed)
