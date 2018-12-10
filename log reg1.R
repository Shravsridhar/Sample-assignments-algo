# Loading the dataset

mowers.df<-read.csv("RidingMowers.csv")

# Running logistic regression on two predictors
logit1.reg<-glm(Ownership~.,data = mowers.df,family = "binomial")
summary(logit1.reg)

# Percentage of household in the study that were owners of a riding mower
summary(mowers.df)

# Summary gives the number of nonowners and owners to be 12 and 12, which shows that its 50 % who are owners.

# Creating a scatter plot with both the predictor variables

# Using ggplot
library(ggplot2)
ggplot(mowers.df,aes(x=Income,y=Lot_Size))+geom_point(color="red",size=2)

plot(mowers.df$Income ,mowers.df$Lot_Size,col = c("yellow", "red")[mowers.df$Ownership], xlab = "Income", ylab = "Lot Size", pch = 2)
legend(x = "topright", legend = levels(mowers.df$Ownership),col = c("yellow", "red") , pch = 2)

# Owner Class seem to have higher average income

# Percentage of household classified correctly among non owners : 
pred.df <- predict(logit1.reg, mowers.df, type = "response")
table(ifelse(pred.df > 0.5, 1, 0), mowers.df$Ownership)

# From the Confusion matrix, it can be observed that 10 out of 12 are classified correctly as non owners which is ~83%

# Decreasing cutoff value 
table(ifelse(pred.df > 0.4, 1, 0), mowers.df$Ownership)
# Increasing the cutoff value
table(ifelse(pred.df > 0.6, 1, 0), mowers.df$Ownership)

# Comparing both the tables, it is clearly visible that the cutoff is to be increased to increase the percentage of correctly classified non owners

# Findind odds for a new tract 
newtract.df <- data.frame(Income = 60, Lot_Size = 20)
newpred.df <- predict(logit1.reg, newtract.df, type = "response")
newpred.df

logit1 <- -25.9382 + 0.1109*(newtract.df$Income) + 0.9638*(newtract.df$Lot_Size)
odds1 <- sapply(logit1, exp)
odds1

# Classifying the new tract - Owner
newpred.df
prob1 <- (odds1/(odds1 + 1))
prob1



