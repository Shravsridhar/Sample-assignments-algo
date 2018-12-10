# Comparision with Logistic Regression
bank.df<-read.csv("Universalbank.csv")

# Excluding the feature Zip Code
bank.df=bank.df[,-c(5)]
bank.df$Undergraduation[bank.df$Education=="1"] <- 1
bank.df$Undergraduation[bank.df$Education!="1"] <- 0
bank.df$Graduation[bank.df$Education=="2"] <- 1
bank.df$Graduation[bank.df$Education!="2"] <- 0
bank.df$Doctorate[bank.df$Education=="3"] <- 1
bank.df$Doctorate[bank.df$Education!="3"] <- 0

# Converting categorical variables with more than 2 categories into dummy variable
bank.df$Education
cols  <- c("PersonalLoan","Undergraduation","Graduation","Doctorate","SecuritiesAccount","CDAccount", "Online", "CreditCard") 
bank.df[cols]<-lapply(bank.df[cols], factor)
bank.df$PersonalLoan<-as.factor(bank.df$PersonalLoan)

train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
traindata = bank.df[train.index, ] 
validdata = bank.df[-train.index, ]

logis.reg = glm(PersonalLoan ~ ., data = traindata, family = binomial(link = "logit"))
options(scipen = 999) #  use options() to ensure numbers are not displayed in scientific notation.
summary(logis.reg)

# predicting for validation data
library(forecast)
predicted_loan = predict(logis.reg, validdata[, -8], type = "response") # for testing set
predicted_loan = round(predicted_loan, 3)
data.frame("Probability" = predicted_loan[1:20], "Predicted_Acceptance" = ifelse(predicted_loan[1:20] >= 0.5, 1, 0), "Actual_Acceptance" = validdata$PersonalLoan[1:20]) 

# confusion matrix 
confusionmatrixTe = table(ifelse(predicted_loan >= 0.5, 1, 0), validdata$PersonalLoan)


# Lift chart
library(gains)

pred.LDA <- data.frame(validdata$PersonalLoan, predicted_loan)
colnames(pred.LDA) <- c("target","score")
lift.LDA <- lift(target ~ score, data = pred.LDA, cuts=10, class="1")
xyplot(lift.LDA, main="Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))



# Decile lift chart
gain=gains(valid.df$PersonalLoan,predicted_loan)

m<-mean(valid.df$PersonalLoan)
heights <- gain$mean.resp/m
midpoints <- barplot(heights, names.arg = gain$depth,
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

gain = gains(validdata$PersonalLoan, predicted_loan, groups = length(predicted_loan))
plot(c(0,gain$cume.pct.of.total*sum(validdata$PersonalLoan))~c(0,gain$cume.obs), xlab="# cases", ylab="Cumulative", main="", type="l") 
lines(c(0,sum(validdata$PersonalLoan))~c(0, dim(validdata)[1]), lty=2)

# Accuracy
CVbinary(logis.reg)

# Confusion matrix
table(ifelse(logstc.pred > 0.5, 1, 0), valid.df$Personal.Loan)


