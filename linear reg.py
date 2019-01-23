import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def coef_reg(x,y):
    n = np.size(x)
    meanx = np.mean(x)
    meany = np.mean(y)
    num = np.sum(y*x) - n*meany*meanx 
    den = np.sum(x*x) - n*meanx*meanx 
    b_1 = num / den
    b_0 = meany - b_1*meanx 
    return(b_0, b_1)
    
def plot_line(x,y,b):
    plt.scatter(x,y,color ="b",s=20)
    resp = b[0] + b[1]*x
    plt.plot(x,resp,color="r")
    plt.xlabel('x')
    plt.ylabel('y')
    plt.show()
        
    
def file_load(path):
	df = pd.read_csv(path)
	return df
    
def main():
    x = np.array([0,1,2,3,4,5,6,7,8,9])
    y = np.array([0,1,1,1,0,0,0,1,0,0])
    a = coef_reg(x,y)
    print("Coefficients:\n b_0 = {}\n b_1 = {}".format(a[0],a[1]))
    plot_line(x,y,a)
    