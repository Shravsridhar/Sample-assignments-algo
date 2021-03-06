import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler  
from sklearn.neighbors import KNeighborsClassifier, KNeighborsRegressor 
from sklearn.metrics import confusion_matrix,accuracy_score,classification_report,explained_variance_score, mean_squared_error

# Load data
df= pd.read_csv('BostonHousing.csv')

# split data into predictors and class
X = df.loc[:,"CRIM":"LSTAT"].values  
y = df.loc[:, "MEDV"].values  

# split training and valildation data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.40) 

# normalize data - feature scaling
scaler = StandardScaler()  
scaler.fit(X_train)
X_train = scaler.transform(X_train)  
X_test = scaler.transform(X_test)  

# train data
reg = KNeighborsRegressor(n_neighbors=5)  
reg.fit(X_train,y_train)  

# predict validation data
y_pred = reg.predict(X_test)    

# choose best k ranging from 1 to 5
for i in range(1,6):
 neigh = KNeighborsRegressor(n_neighbors = i, weights='uniform', algorithm='auto')
 neigh.fit(X_train, y_train) 
 y_pred = neigh.predict(X_test)
 acc = neigh.score(X_test,y_test)
 df=pd.DataFrame({"Accuracy":  [acc],"K Value": [i]})
 print(df)
 
 # k value 2 has the highest accuracy, hence choose k=2 for the further preceedings

 # error rate  of training set
 mean_squared_error(y_train,reg.predict(X_train))
 
# validation data error
for i in range(1,6):
    neigh = KNeighborsRegressor(n_neighbors = i, weights='uniform', algorithm='auto')
    neigh.fit(X_train, y_train) 
    y_pred = neigh.predict(X_test)
    error = mean_squared_error(y_test,y_pred)
    df1= pd.DataFrame({"K Value": [i],"Error": [error]})
    print(df1)
# predict value for new data
new_data= np.array([0.2,0,7,0,0.538,6,62,4.7,4,307,21,10]).reshape(1,-1)
y_pred1 = neigh.predict(new_data)

 


 
