# Loading the data
Uni_bank<-read.csv("Universalbank.csv")

# Excluding the feature Zip Code
Uni_bank=Uni_bank[,-c(5)]

# Converting categorical variables with more than 2 categories into dummy variable
# Converting predictor into factor
Education<-c(1,2,3)
Uni_bank$Education<-as.factor(Uni_bank$Education)

# Creating dummy variable using the package dummies

Uni_bank$Undergraduation[Uni_bank$Education=="1"] <- 1
Uni_bank$Undergraduation[Uni_bank$Education!="1"] <- 0
Uni_bank$Graduation[Uni_bank$Education=="2"] <- 1
Uni_bank$Graduation[Uni_bank$Education!="2"] <- 0
Uni_bank$Doctorate[Uni_bank$Education=="3"] <- 1
Uni_bank$Doctorate[Uni_bank$Education!="3"] <- 0

# Excluding Education and ID
Uni_bank=Uni_bank[,-c(1,7)]
# Converting categorical variables into factors
cols  <- c("PersonalLoan","Undergraduation","Graduation","Doctorate","SecuritiesAccount","CDAccount", "Online", "CreditCard") 
Uni_bank[cols]<-lapply(Uni_bank[cols], factor)
Uni_bank$PersonalLoan<-as.factor(Uni_bank$PersonalLoan)

# Numerical variables - ID,Age,Experience,Income,Family,CCAvg,Mortage

# Partition the data into training and test set
sample.df = sample(c(1:dim(Uni_bank)[1]), dim(Uni_bank)[1]*0.6)
train.df = Uni_bank[sample.df,]
valid.df= Uni_bank[-sample.df,]

# Numerical predictor variables - mean and standard deviation 
apply(train.df[,c(1:6)],2,mean)
apply(train.df[,c(1:6)],2,sd)

# Categorical predictor variables - percentages/ classification
table(train.df[,c(8:14)])

# Building LDA model
library(MASS)
lda.reg<-lda(PersonalLoan~Age+Experience+Income+Family+CCAvg+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard+Undergraduation+Graduation+Doctorate,data = train.df)
lda.train.pred<-predict(lda.reg,data = valid.df)
lda.class<-lda.train.pred$class
table(lda.class,train.df$PersonalLoan)

# plot shows how the response class has been classified as group 1 and 0
ldahist(lda.train.pred$x[,1], g= lda.train.pred$class)

# Examining the model performance on validation set
# Accuray rate 
valid.lda = predict(lda.reg, newdata=valid.df)
mean(valid.lda$class == valid.df$PersonalLoan)

# Misclassification
table(Predicted=valid.lda$class,Actual=valid.df$PersonalLoan)

# Probability of validation dataset
valid.lda

# lift chart
pb <- NULL
pb <- valid.lda$posterior
pb <- as.data.frame(pb)
pred.LDA <- data.frame(validdata$PersonalLoan, pb$'1')
colnames(pred.LDA) <- c("target","score")
lift.LDA <- lift(target ~ score, data = pred.LDA, cuts=10, class="1")
xyplot(lift.LDA, main="Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

# Decile lift chart
library(gains)
gain=gains(valid.df$PersonalLoan,pb$`1`)
m<-mean(valid.df$PersonalLoan)
heights <- gain$mean.resp/m
midpoints <- barplot(heights, names.arg = gain$depth,
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# Minimizing and Maximizing cutoff

prior1<-c(0.2,0.8)
lda.reg1 <- lda(PersonalLoan ~ ., data = train.df, CV=FALSE, prior = prior1)
lda.test1<-predict(lda.reg1,data = train.df)
table(Predicted= lda.test1$class,Actual=train.df$PersonalLoan)

prior2<-c(0.8,0.2)
lda.reg2 <- lda(PersonalLoan ~ ., data = train.df, CV=FALSE, prior = prior2)
lda.test2<-predict(lda.reg2,data = train.df)
table(Predicted= lda.test2$class,Actual=train.df$PersonalLoan)

sum(lda.train.pred$posterior [ ,1] > .4)

sum(lda.train.pred$posterior [ ,1] > .6)
