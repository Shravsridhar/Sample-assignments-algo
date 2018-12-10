# Loading the bank data
bank.df<- read.csv("banks.csv")

# Factoring the outcome variable
bank.df$Financial.Condition <- as.factor(bank.df$Financial.Condition)

# Running logistic regression on the entire dataset with two predictors
logit.reg<-glm(Financial.Condition~TotExp.Assets+TotLns.Lses.Assets,data = bank.df,family = "binomial")
summary(logit.reg)
coef(logit.reg)
# Estimated equations
# logit <- -14.721 + 89.834*(TTotExp.Assets) + 8.371*(TotLns.Lses.Assets)
# odds <- exp(-14.721 + 89.834*(TTotExp.Assets) + 8.371*(TotLns.Lses.Assets))
# prob <- (odds/(1 + odds))

library(forecast)
Newdata.df <- data.frame(TotExp.Assets = 0.11, TotLns.Lses.Assets = 0.6)
newpredict.df <- predict(logit.reg, Newdata.df, type = "response")
cat("Financial Status of new data",newpredict.df)

# Calculating the logit, odds and probability for the new data tract given
logit <- -14.721 + 89.834*(Newdata.df$TotExp.Assets) + 8.371*(Newdata.df$TotLns.Lses.Assets)
odds <- sapply(logit, exp)
prob <- (odds/(1 + odds))

# Computing threshold and logit 

# The probability cutoff value of 0.5 is used as a threshold to seperate both the classes,
# hence, the odds threshold cutoff value should be odds/(1+odds) = 0.5. 
# which implies, odds = 1. if odds = 1, then log(odds) = 0. 
# Result : The cutoff value for odds is 1 and the logit value's cutoff value if 0.

# Interpreting the estimated Coefficient 
# The odds of bank being classified as weak increases by a factor of e^89.834 for a unit 
#increase in TotExp.Assets
# e^89.834 = 1.0296

# Misclassification 
pred.df <- predict(logit.reg, bank.df, type = "response")
table(ifelse(pred.df > 0.5, 1, 0), bank.df$Financial.Condition)

table(ifelse(pred.df > 0.6, 1, 0), bank.df$Financial.Condition)

pred1.df <- as.data.frame(pred.df)
pred1.df$actual <- bank.df$Financial.Condition
colnames(pred1.df) <- c("Predicted Value", "Actual value")
pred1.df

#from the matrix, we get 2 misclassifications and we can see from predicted values with probability that the cutoff value to classify is 0.467.


# Constructing a confusion matrix with cutoff as 0.45
table(ifelse(pred.df > 0.45, 1, 0), bank.df$Financial.Condition)

# Misclassification is seen to be reduced in this case
# Hence, to minimize the expected cost of misclassification, the cutoff cost should be decreased



