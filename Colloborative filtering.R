library(recommenderlab)


# Loading data
course<-read.csv("Coursetopics.csv")

# Utility Matrix with Rating data
rating.mat <-as.matrix(course)

data.df<-sapply(data.frame(rating.mat), as.numeric)
rate.df <- as(data.df, "realRatingMatrix")

# Building user Recommendation system
User_Rec <- Recommender(rate.df, "UBCF")

# Prediction
pred <- predict(User_Rec, rate.df, type="ratings")
as(pred,"matrix")

# A null matrix is obtained
# Possible Reasons: 
#.	CourseTopics.csv file has binary valued attributes, but the recommendation system uses "Rating" matrix.
#.	It might be because the Item and User have never interacted.
#.	Cold start problem with the user- based recommendation system.
#.	It is difficult to find users with the same interest in this case.

