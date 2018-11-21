

path="E:/Work/November/Heater/Heater_Data orgonized"
import pandas as pd
import numpy as np 
import pylab as plt
import seaborn as sns; sns.set()
import keras
from keras.datasets import mnist
from keras.models import Sequential, Model
from keras.layers import Dense
from keras.optimizers import Adam
import csv
import os
from functools import reduce
from math import sqrt
#from numpy import concatenate
from matplotlib import pyplot
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from numpy import concatenate 
from pandas import concat
from keras.layers import Dense,LSTM,ConvLSTM2D
from keras.layers import Conv2D,MaxPooling2D,Flatten,TimeDistributed,Conv1D,MaxPooling1D
from pandas import DataFrame
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import MinMaxScaler
from math import sqrt
from sklearn.metrics import mean_squared_error
from keras.layers import Dropout


os.chdir(path)
filnames=os.listdir(path)
frame = pd.DataFrame()
list_ = []
for file_ in filnames:
    df = pd.read_excel(file_, header=0)
    list_.append(df)
heater_data_air_A = pd.concat(list_)

def whole_merge(path):
    big_filenames=os.listdir(path)
    list_big=[]
    for i in range(0,len(big_filenames)):
        p=path+'/'+big_filenames[i]
        os.chdir(p)
        filnames=os.listdir(p)
        output = pd.DataFrame()
        list_ = []
        for file_ in filnames:
           print(file_)
           df = pd.read_excel(file_, header=0)
           list_.append(df)
        output = pd.concat(list_)
        list_big.append(output)
    dataframe=pd.concat(list_big,axis=1,join="inner")
    return(dataframe)


heater_data=whole_merge(path)  

