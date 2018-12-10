# Loading the data
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
# Partition the data into training and test set
sample.df = sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df = bank.df[sample.df,]
valid.df=bank.df[-sample.df,]


# Performing Discriminant analysis to model personal loan as a function of remaining predictors
library(DiscriMiner)
da.reg = linDA(train.df[,-9],train.df[,9])
colnames(da.reg$functions)=c("Personal Loan Rejection","Personal Loan Acceptance")
da.reg$functions

# Computing summary statistics for both classes 
summary(da.reg$functions)
# There are no categorical predictors. All the predictors are numerical : continuous
# Mean and standard deviation of all the continuous predictors
apply(train.df[,-9],2,mean)
apply(train.df[,-9],2,sd)

# Predictors where the two classes differ substantially - CD.Account,Securities Account

# Examining the model performance on validation set
# Accuray rate of model
library(MASS)
library(DAAG)

confusion(train.df$Personal.Loan, da.reg$classification)
confusionMatrix(as.factor(train.df$Personal.Loan),as.factor( da.reg$classification))

# Table to show the percentage of correct classifications
# Two ways
# confusion matrix
da.reg$confusion
table1 <- table(train.df$Personal.Loan,da.reg$classification)
table1
#table1.bind <- rbind(table1[1, ]/sum(table1[1, ]), table1[2, ]/sum(table1[2, ]))
#dimnames(table1.bind) <- list(Actual = c("0", "1"), "Predicted " = c("0", "1"))
#print(round(table1.bind, 3))


# The above ouput table specifies that one type of misclassification is more than the other
# Misclassification of Non acceptors as acceptors is more

diag(prop.table(class_table, 1))

# Accuracy rate of model being the total percent of correct predictions
sum(diag(prop.table(class_table)))



# Lift Chart and Decile lift charts
library(gains)
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
nm=as.data.frame(lapply(valid.df,normalize))
reg=linDA(nm[,-9],nm[,9])
propensity.accept=exp(reg$scores[,1:2])/(exp(reg$scores[,1])+exp(reg$scores[,2]))
result = data.frame(Actual=valid.df$Personal.Loan,reg$classification,reg$scores, propensity.accept=propensity.accept)
head(result)
# Decile lift chart 
gain_log=gains(result$propensity.accept.1,valid.df$Personal.Loan)
barplot(gain_log$mean.resp/mean(valid.df$Personal.Loan),names.arg=gain_log$depth,xlab = "Percentile",ylab = "Mean Response",main = "Decile-lift chart")
# Lift chart   
plot(c(0,gain_log$cume.pct.of.total*sum(result$Actual))~c(0,gain_log$cume.obs),xlab="cases",ylab="Cumulative",type="l",main="Lift Chart")
lines(c(0,sum(result$Actual))~c(0,dim(result)[1]),col="gray",lty=2)               

# Comparing discriminant anaysis with logistic regression
logstc=glm(Personal.Loan~.,data=train.df,family="binomial")
logstc.pred=predict(logstc,valid.df[,-9],type="response")
options(scipen=999)
summary(logstc)
gain <- gains(valid.df$Personal.Loan, logstc.pred, groups=length(logstc.pred))

# lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)
# Decile Lift Chart
heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# Accuracy
CVbinary(logstc)
accuracy(logstc.pred, valid.df$Personal.Loan)

# Confusion matrix
table(ifelse(logstc.pred > 0.5, 1, 0), valid.df$Personal.Loan)

# Predictive accuracy increases with logistic regression

# To reduce cost of misclassification of acceptor as non acceptor
# Increasing the cutoff of classifying as 1 into 0.8
prior<-c(0.8,0.2)
lda.reg <- lda(Personal.Loan ~ ., data = train.df,CV = TRUE, prior = prior)
confusion(train.df$Personal.Loan, lda.reg$class, prior = prior)
table(train.df$Personal.Loan, lda.reg$class)

# Decreasing the cutoff of classifying as 1 into 0.2
prior1<-c(0.2,0.8)
lda.reg1 <- lda(Personal.Loan ~ ., data = train.df,CV = TRUE, prior = prior1)
confusion(train.df$Personal.Loan, lda.reg1$class, prior = prior1)
table(train.df$Personal.Loan, lda.reg1$class)

# From the above two comparisions, it is visible that increasing the cutoff gives better accuracy.


      
