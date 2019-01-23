import pandas as pd 
import numpy as np


# Load file
def file_load(path):
	df = pd.read_csv(path)
	return df

 
df_data = file_load("E:\\course\\ML\\codes\\datasets\\RidingMowers.csv")


# prior prob of Owner and NonOwner
total = df_data["Ownership"].count()
no_0 = df_data["Ownership"][df_data["Ownership"]=='Nonowner'].count()
no_1 = df_data["Ownership"][df_data["Ownership"]=='Owner'].count()

def prior_prob(no_0,no_1,total):
    p_0 = no_0/total
    p_1 = no_1/total
    return p_0,p_1

prior_class0, prior_class1 = prior_prob(no_0,no_1,total)

# likelihood 
class_mean = df_data.groupby('Ownership').aggregate('mean')
class_var = df_data.groupby('Ownership').aggregate('var')
class_std = df_data.groupby('Ownership').aggregate('std')

# split into train and test data
def split_trte(df_data,ratio):
    tr_size = int(len(df_data) * ratio)
    df_train = df_data.iloc[0:tr_size]
    df_test = df_data.iloc[tr_size:]
    return [df_train,df_test]


df_train,df_test = split_trte(df_data,0.60)

# separate train and test data with labels
n_tr = len(df_train.columns)-1
n_te = len(df_test.columns)-1
df_train_label = df_train[df_train.columns[n_tr]]
df_test_label = df_test[df_test.columns[n_te]]
df_train = df_train.iloc[:,:-1]
df_test = df_test.iloc[:,:-1]

# pdf
def cal_prob(x, mean, var):
	expo = np.exp((-(x-mean)**2)/(2*var))
	return  1 /(np.sqrt(2*np.pi*var)) * expo
   
   
# ex find prob of p(NonOwner|Income,LotSize)
prob_inc_0 = cal_prob(df_test['Income'],class_mean['Income'][class_var.index == 'Nonowner'].values[0],class_var['Income'][class_var.index =='Nonowner'].values[0]) 
prob_lot_0 = cal_prob(df_test['Lot_Size'],class_mean['Lot_Size'][class_var.index == 'Nonowner'].values[0],class_var['Lot_Size'][class_var.index =='Nonowner'].values[0])
pdf_0 = prior_class0 * prob_inc_0 * prob_lot_0
# avg
#pdf_0 = sum(pdf_0)/len(pdf_0)

prob_inc_1 = cal_prob(df_test['Income'],class_mean['Income'][class_var.index == 'Owner'].values[0],class_var['Income'][class_var.index =='Owner'].values[0]) 
prob_lot_1 = cal_prob(df_test['Lot_Size'],class_mean['Lot_Size'][class_var.index == 'Owner'].values[0],class_var['Lot_Size'][class_var.index =='Owner'].values[0])
pdf_1 = prior_class1 * prob_inc_1 * prob_lot_1
# avg
#pdf_1 = sum(pdf_1)/len(pdf_1)

# Accuracy
# find class that test data belongs to from the pdf of belonging to each class
pred_prob = pd.DataFrame()
def pred_class(pdf_0,pdf_1):
    pred_prob = (pdf_1 >= pdf_0)
    pred_prob = pred_prob * 1
    return pred_prob
    
pred_prob = pred_class(pdf_0,pdf_1)

# label encoding : Owner 1 , NonOwner 0
#df_data["Ownership"] = df_data["Ownership"].astype('category')
#df_data["Ownership"] = df_data["Ownership"].cat.codes

# convert categorical to numerical values for comparison
df_test_label = df_test_label.astype('category')
df_test_label = df_test_label.cat.codes
#pred_prob = pred_prob.astype('category')
#pred_prob = pred_prob.cat.codes


def perf_acc(df_test_label,pred_class):
    cor_clas = 0
    for x in range(len(df_test_label)):
        if df_test_label[x][-1] == pred_class[x]:
            cor_clas += 1
    accuracy = (cor_clas/(len(df_test_label))) * 100
    return accuracy

result = df_test_label == pred_prob
result  = result.astype('category')
result = result.cat.codes

def perf_accuracy(result):
    cor_clas = 0
    cor_clas = np.sum(result)
    accuracy = (cor_clas/(len(df_test_label))) * 100
    return accuracy

perf_accuracy(result)

