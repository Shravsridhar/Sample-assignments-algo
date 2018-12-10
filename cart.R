library(rpart)
library(rpart.plot)
library(ModelMetrics)


car.df<-read.csv("ToyotaCorolla.csv")
car.df<-car.df[,-1]
# Splitting into training and validation data
train.index<- sample(c(1:dim(car.df)[1]),dim(car.df)[1]*0.6)
train.df<-car.df[train.index,]
valid.df<-car.df[-train.index,]

# Part a
# Building a regression tree

reg.tree <- rpart(Price~ Age_08_04+ KM+ Fuel_Type+ HP+ Automatic+ Doors+ Quarterly_Tax+ Mfr_Guarantee+ Guarantee_Period+ Airco+ Automatic_airco+ CD_Player+ Powered_Windows+ Sport_Model+ Tow_Bar,
                 data=train.df,control = rpart.control(minbucket = 1, maxdepth = 30, cp = 0.001),  method = "anova")

# Plotting the tree
prp(reg.tree, type = 1, extra = 1, split.font = 2,cex= 0.41,varlen = -10, box.palette = "Green", round = 0, 
    leaf.round = 10)

# Printing the table
printcp(reg.tree)

# Part i From the output obtained, it can be concluded that Age_08_04, Airco and Automatic  and KM appear to be most important car specifications.

# Part ii
pred.train <- predict(reg.tree, train.df[-2])
rmse(pred.train, train.df$Price)

pred.valid <- predict(reg.tree, valid.df[-2])
rmse(pred.valid,valid.df$Price)

# It is observed that RMSE value of predicted dataset is more than training data.

boxplot(rmse(pred.train,train.df$Price),rmse(pred.valid,valid.df$Price),names=c("Training data","Validation data"),main="RMS Error")


boxplot(pred.train, pred.valid ,names=c("Training data","Validation data"),main="Predicted Values Comparision")

# The RMS Error for training data is less than validation data. This shows a good fit.

# Part iii 
#  RMS Error shows that tree predictions are not quite well as we have used some pruning parameters.
# If these parameters are not present, then the tree would give a rms error with lesser value.
# The validation error is more compared to the training error , this indicates that predictive performance is less when compared to training set. This is because tree overfits the validation data.

# Part iv
# Pruning the full tree

cv.tree <- rpart(Price~ Age_08_04+ KM+ Fuel_Type+ HP+ Automatic+ Doors+ Quarterly_Tax+ Mfr_Guarantee+ Guarantee_Period+ Airco+ Automatic_airco+ CD_Player+ Powered_Windows+ Sport_Model+ Tow_Bar,
                 data=train.df, control = rpart.control(minbucket = 1, maxdepth = 30, cp = 0.001),method = "anova",cp = 0.001, xval = 5)

pruned.tree<- prune(cv.tree,cp=cv.tree$cptable[which.min(cv.tree$cptable[,"xerror"]),"CP"])
length(pruned.tree$frame$var[pruned.tree$frame$var == '<leaf>'])
prp(pruned.tree,type=1,extra=1,split.font = 1,varlen = -10)

# Prediction
pred.train <- predict(pruned.tree, train.df[-2])
rmse(pred.train, train.df$Price)

pred.valid <- predict(pruned.tree, valid.df[-2])
rmse(pred.valid,valid.df$Price)

# Comparing the predictive performance of the pruned tree with validaion set, it is observed that the rmse for training data is still less than the validation data.
# In this case, pruning the tree didnt bring about much change in predictive performance of validation dataset.

# Part b

# Binning the numeric data into categorical variable
range(car.df$Price)
bin.price <- cut(car.df$Price, breaks = seq(4349,32501, by= 1407.6)) 
car.df$PriceBin <- bin.price
car.new.df <- car.df[-2]

# Splitting into training and validation dataset
index.new <- sample(2, nrow(car.new.df), replace=TRUE, prob=c(0.6, 0.4))
train.new.df <- car.new.df[index.new==1,]
valid.new.df <- car.new.df[index.new==2,]

# Building a classification tree

clas.tree <- rpart(PriceBin~Age_08_04+ KM+ Fuel_Type+ HP+ Automatic+ Doors+ Quarterly_Tax+ Mfr_Guarantee+ Guarantee_Period+ Airco+ Automatic_airco+ CD_Player+ Powered_Windows+ Sport_Model+ Tow_Bar,
                  data=train.new.df, control = rpart.control(minbucket = 1), method = "class")

prp(clas.tree, type = 1, extra = 1, split.font = 2, varlen = -10, box.palette = "Blue", round = 0,   leaf.round = 10)
summary(clas.tree)
printcp(clas.tree)

# Part i
# The classification tree built is different from the regression tree. The top predictors in classification tree are Age, Fuel_type and HP. It can be noticable that they are different from the regression tree result. Also, Regression tree gives the exact value but the classifacation gives a range.
# Add the reason of wh they differ also

# Predict price for a new tract

# New tract

newdata.df <- data.frame(Age_08_04 =77, KM = 117000, Fuel_Type = "Petrol", HP = 110, Automatic = 0, 
                         Doors = 5, Quarterly_Tax = 100,
                         Mfr_Guarantee = 0, Guarantee_Period = 3, Airco = 1, Automatic_airco = 0, CD_Player = 0, 
                         Powered_Windows = 0, Sport_Model = 0, Tow_Bar = 1)

# Predicting using classification tree

pred.new.clas <- predict(clas.tree, newdata.df, type = "class")
pred.new.clas

# Predicting using regression tree
pred.new.reg <- predict(reg.tree , newdata.df)
pred.new.reg

# From the above predictions, it is observed that Regression tree gives the average of the values present in the leaf node. whereas, classification tree gives the range to which new data may lie.
# predictors used : same for both case. add advantages and dissadvantages of both methods




