# Loading the data
bank.df<-read.csv("Universalbank_1500.csv")

# Partitioning the data into training and validation sets
selected.var <- c(1,10, 13, 14)
predictors<-c(10,13,14)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, selected.var]
train_data<-bank.df[train.index,predictors]
valid.df <- bank.df[-train.index, selected.var]

# creation of pivot table for training data
library(reshape)
data = melt(train.df, id.vars = c("CreditCard","Personal.Loan","Online"), 
                     measure.vars = c("ï..ID"))
cast(data,CreditCard+ Personal.Loan ~ Online, value  = "count")

# creation of two pivot tables 
data1 = melt(train.df, id.vars = c("Personal.Loan","Online"), 
            measure.vars = c("ï..ID"))
cast(data1,Personal.Loan ~ Online, value  = "count")


data2 = melt(train.df, id.vars = c("Personal.Loan","CreditCard"), 
             measure.vars = c("ï..ID"))
cast(data2,Personal.Loan ~ CreditCard, value  = "count")

# Running naive bayes on the training data
library(e1071)
nb<-naiveBayes(Personal.Loan~.,data = train_data)
# predict prob
pred.prob <- predict(nb, newdata = valid.df, type = "raw")

# conditional probability tables
prop.table(table(train_data$Personal.Loan,train_data$Online),margin = 1)
prop.table(table(train_data$Personal.Loan,train_data$CreditCard),margin = 1)

#predict class membership
pred.class <- predict(nb, newdata = valid.df)

# predict data with Loan =1, CC=1, Online =1
df.new<-data.frame("Personal.Loan"=1,"Online"=1,"CreditCard"=1)
new.prob <- predict(nb, newdata = df.new, type = "raw")
new.prob