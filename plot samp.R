#Load data
twitter.df<-read.csv("M01_quasi_twitter.csv")
View(twitter.df)

#Data distribution for friends_count variable
table(twitter.df$friends_count)
barplot(twitter.df$friends_count,main = "Distribution of friends_count",xlab = "friends count",ylab = "frequency")
hist(twitter.df$friends_count,col='blue',main = "Distribution of friends_count",xlab = "friends count")

#Summary statistics on friends_count variable
summary(twitter.df$friends_count)

#Data Quality in friends_count variable
#dataQualityR package used for analyzing the data quality of variables
#checks including missing values, unique values,frequency tables, and generates sumamry statistics
library(dataQualityR)
num.file<-paste(tempdir(),"dq_num_file.csv",sep = "")
cat.file<-paste(tempdir(),"dq_cat_file.csv",sep="")
data.frame(twitter.df, row.names = NULL, check.rows = FALSE,
                                 check.names = TRUE, fix.empty.names = TRUE,
                                 stringsAsFactors = default.stringsAsFactors())

checkDataQuality(twitter.df, out.file.num = num.file,out.file.cat = cat.file,numeric.cutoff = -1)

#3D scatter plot
library(scatterplot3d)
attach(twitter.df)
scatterplot3d(created_at_year,education, age,pch = 20, highlight.3d = TRUE,type = 'h', main = "3D Scatter Plot")

#pie chart
par(mfrow=c(1,2))
slices<-c(650,1000,900,300,14900)
labels<-c("UK","Canada","India","Australia","US")
pc<-round(slices/sum(slices)*100)
labels<-paste(labels,pc)
labels<- paste(labels,"%", sep = "")
pie(slices,labels,main = "Percentage Pie Chart")

#3D pie chart
library(plotrix)
slice<-c(650,1000,900,300,14900)
label<-c("UK","Canada","India","Australia","US")
pie3D(slice,main ="3D Pie Chart",labels = label, explode = 0.1)

#Kernel density plot
par(mfrow=c(1,1))
d<-density(created_at_year)
plot(d,main = "Kernel density of created_at_year",ylab = "density",xlab = "Created at year")
rug(created_at_year) # to plot the location of data values
polygon(d,col="yellow",border="red")
summary(d)




