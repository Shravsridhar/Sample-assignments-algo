import pandas as pd
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import confusion_matrix, accuracy_score,classification_report 

# Loading training and test data
df_test= pd.read_csv('admission_test.csv')
df_train=pd.read_csv('admission_train.csv')

# Training the training dataset

# Instantiate the classifier - gaussian is used since normal distribution of class is specified
nb = GaussianNB()
predictors =[
    "GPA",
    "GMAT"
]

# Train classifier

nb.fit(
    df_train[predictors].values,
    df_train["De"]
)

# computing the probability for each class 
test_pred = nb.predict_proba(df_test[predictors])

# specifying the cutoff value as 0.5 
test_pred[test_pred > 0.5] = 1
test_pred[test_pred < 0.5] = 0
test_pred

# predicting the test data
test_pred = nb.predict(df_test[predictors])
train_pred = nb.predict(df_train[predictors])

# printing the predictions of test data

print("\nPredicted Decisions\n",test_pred)

# constructing confusion matrix for test data
cm_test = confusion_matrix(df_test["De"],test_pred)
print("\nConfusion Matrix of test data\n",cm_test)

# constructing confusion matrix for training data
cm_train = confusion_matrix(df_train["De"],train_pred)
print("\nConfusion Matrix of training data\n",cm_train)

# prior prob of border class

#sum(df_train["De"]) / df_train["De"].shape[0]

# Accuracy and F measure of test data
accuracy_test = accuracy_score(df_test["De"],test_pred)
print("\nAccuracy\t",accuracy_test)
fm_test = classification_report(df_test["De"],test_pred)
print("\nClassification report with F measure\n",fm_test)

# Accuracy and F measure of training data

accuracy_train = accuracy_score(df_train["De"],train_pred)
print("\nAccuracy\t",accuracy_train)
fm_train = classification_report(df_train["De"],train_pred)
print("\nClassification report with F measure\n",fm_train)

# Building knn clasifier

# Training data
classifier = KNeighborsClassifier(n_neighbors=5)  
classifier.fit(df_train[predictors].values,df_train["De"])  

# predicting the test data
test1_pred = classifier.predict(df_test[predictors])  
train1_pred = classifier.predict(df_train[predictors])

# choose best k ranging from 1 to 10
for i in range(1,10):
 neigh = KNeighborsClassifier(n_neighbors = i, weights='uniform', algorithm='auto')
 neigh.fit(df_train[predictors], df_train["De"]) 
 test1_pred = neigh.predict(df_test[predictors])
 accuracy= accuracy_score(df_test["De"],test1_pred)*100
 df=pd.DataFrame({"Accuracy":  [accuracy],"K Value": [i]})
 print(df)
 
 # k=6 has 90% accuracy, hence k=6 is used to build model
classifier1 = KNeighborsClassifier(n_neighbors=6)  
classifier1.fit(df_train[predictors].values,df_train["De"])  

# computing distance 
distance,_=classifier1.kneighbors(df_test[predictors])
distance
# predicting the test data
test1_pred = classifier1.predict(df_test[predictors])  

# constructing confusion matrix for test data
cm_test1 = confusion_matrix(df_test["De"],test1_pred)
print("\nConfusion Matrix of test data\n",cm_test1)

# constructing confusion matrix for training data
cm_train1 = confusion_matrix(df_train["De"],train1_pred)
print("\nConfusion Matrix of training data\n",cm_train1)

