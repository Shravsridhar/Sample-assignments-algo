import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import MultinomialNB
import os
print(os.getcwd())
# Load data
df= pd.read_excel('Universalbank_1500.xlsx')
# split training and valildation data
df_train,df_test=train_test_split(df,test_size=0.4)

# create pivot table
pd.pivot_table(df_train,index=["CreditCard","Personal Loan"], columns = ["Online"],values = ["ID"],aggfunc="count")

# create two pivot tables for training data
pd.pivot_table(df_train,index=["Personal Loan"], values=["ID"],columns= ["CreditCard"],aggfunc="count")
pd.pivot_table(df_train,index=["Personal Loan"], values=["ID"],columns=["Online"],aggfunc="count")
    
# run naive bayes 
nb = MultinomialNB(alpha=1,fit_prior = True)
used_features =[ "Online","CreditCard","ID"]

# Train classifier
nb.fit(
    df_train[used_features].values,
    df_train["Personal Loan"]
)

nb.predict(df_train[used_features])

