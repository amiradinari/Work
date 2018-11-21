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
#####Import each cluster 
#PS the chloride tag should positionated in the fist col just after the time stemp
path="E:/PredictionChloride"
os.chdir(path)
classtwo=pd.read_excel("class2.xlsx",header=0)
classone=pd.read_excel("class1.xlsx",header=0)
classthree=pd.read_excel("class3.xlsx",header=0)
classfour=pd.read_excel("class4.xlsx",header=0)
#Remove the time stemp and call the new data set "Data"
Data=classfour.iloc[:,1:151]
#fix the number of lags 
n_min = 5
#fix the number of features in your data set "Data"
n_features = 150
####"Hybrid model needs the chloride to be in the end of the data set 
#name your chloride tag "Chloride"
Data['Chloride']=classfour.iloc[:,1]
#Remove the chloride tag that was just after the time stemp
Data=Data.drop(Data.columns[[0]],axis=1)
####Removing thbas values in chloride 
#we smoothed in the 30 min data by window equal to 30 so in the 1 min data it will be 30*30
threshold=Data.rolling(window=900).mean()
threshold=Data.iloc[900:,:]
#####take the  smoothed chlordie that only greater than 0.013
#search for the index where the smoothed chloride is greater than 0.013
i=[x for x in range(0,threshold.shape[0]) if threshold.iloc[x,149]>0.013]
#take the data where the chloride has no bad values 
Data=Data.iloc[i,:]

###smoothing and cleaning the data
#window is equal to 05 to smooth
Data=Data.rolling(window=15).mean()
Data=Data.iloc[15:,:]
####dropna is the equivlent for na.omit in R ,so we are cleaning missing data
Data=Data.dropna()


#####Scaling
#set a min max scaler with min=0 and max =1
scaler = MinMaxScaler(feature_range=(0, 1))
#apply the scaler on the "Data.values" which is the matrix of the data set "Data"
scaled = scaler.fit_transform(Data.values)
######Lagging
#this function will be used to lag the data 
def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
	n_vars = 1 if type(data) is list else data.shape[1]
	df = DataFrame(data)
	cols, names = list(), list()
	# input sequence (t-n, ... t-1)
	for i in range(n_in, 0, -1):
		cols.append(df.shift(i))
		names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
	# forecast sequence (t, t+1, ... t+n)
	for i in range(0, n_out):
		cols.append(df.shift(-i))
		if i == 0:
			names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
		else:
			names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
	# put it all together
	agg = concat(cols, axis=1)
	agg.columns = names
	# drop rows with NaN values
	if dropnan:
		agg.dropna(inplace=True)
	return agg
#Apply the lagging function on the scaled vlaues 
reframed = series_to_supervised(scaled, n_min)
####From Now on the data set we will be using for the model are the lagged scaled DATA
values = reframed.values

####mODLING WITH THE HYBRID MODEL
#FIX THE NUMBER OF TRAINING DATA(I took 80% OF THE DATA)
n_train_hours = round(Data.shape[0]*0.8)
#create the training data
train = values[:n_train_hours, :]
#creating the test data

test = values[n_train_hours:, :]
##remove the predicted variable "chloride" (which should be in the end of you training data) in both test and training and call it train_y ,test_y

train_X, train_y = train[:, :-1], train[:, -1]
test_X, test_y = test[:, :-1], test[:, -1]

#######fir your regressin model on the data

from sklearn import linear_model
#lm is a linear reression model
lm = linear_model.LinearRegression()
#we fit it on the training data and give it the chloride as predicted variable 
model = lm.fit(train_X, train_y)
#make you predction on the test data (all the inputs expet for the chloride)
predictions = lm.predict(test_X)
####Now we will mapp back to the original data set
predictions = predictions.reshape((predictions.shape[0], 1))
# invert scaling for test data
#take only the last number of features of the test data set 
#start should be the length of the variable "train" - the number of features "n_features"
start=750
#end should be the length of the variable "train"
end =900
####we merge the predicted chloride with the rest of the data set 
inv_yhat = concatenate(( test_X[: , 750:900], predictions), axis=1)
##we invers the scaling
inv_yhat = scaler.inverse_transform(inv_yhat)
#we take the inversed scaling of the predicted chloride 
inv_yhat = inv_yhat[:,149]
# invert scaling for real chloride 
test_y = test_y.reshape((len(test_y), 1))
#we merge the real chloride with the rest of the data set 
inv_y = concatenate(( test_X[: , 750:900],test_y), axis=1)
# we invers the scaling 
inv_y = scaler.inverse_transform(inv_y)
#we take the inversed scaling of the real chloride 
inv_y = inv_y[:,149]
# calculate RMSE
rmse = sqrt(mean_squared_error(inv_y, inv_yhat))
print('Test RMSE: %.3f' % rmse)
#Calculate R squared
from sklearn.metrics import r2_score
r2_score(inv_y, inv_yhat)

#plot the predicted chloride and real chloride in the same plot 
pyplot.plot(inv_yhat,label="prediction")
pyplot.plot(inv_y,label="real")
pyplot.legend()
pyplot.show()




###Depp learning

# frame as supervised learnin
Data=classfour.iloc[:,1:154]
Data=Data.rolling(window=900).mean()

Data=Data.dropna()
#shift=Data.shift(15)
#shift=shift.dropna()
#spike=remove_spike(Data.values)
scaler = MinMaxScaler(feature_range=(0, 1))
scaled = scaler.fit_transform(Data.values)
reframed = series_to_supervised(scaled, n_hours)

print(reframed.shape)
Data.columns.tolist()
# split into train and test sets
values = reframed.values

n_train_hours = round(Data.shape[0]*0.8)
train = values[:n_train_hours, :]
test = values[n_train_hours:, :]
# split into input and outputs
n_obs = n_hours * n_features
train_X, train_y = train[:, :n_obs], train[:, -n_features]
test_X, test_y = test[:, :n_obs], test[:, -n_features]
print(train_X.shape, len(train_X), train_y.shape)
train_X = train_X.reshape((train_X.shape[0],1, n_hours, n_features,1))
test_X = test_X.reshape((test_X.shape[0],1, n_hours, n_features,1))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)

cnn=Sequential()
cnn.add(TimeDistributed(Conv2D(filters=32, kernel_size=(4,1), activation='relu'), input_shape=(None, train_X.shape[2],train_X.shape[3],1)))
cnn.add(TimeDistributed(Conv2D(filters=64, kernel_size=(2,1), activation='relu')))
cnn.add(TimeDistributed(MaxPooling2D(1,2)))
cnn.add(TimeDistributed(Flatten()))
#cnn.add(LSTM(100, stateful=True,return_sequences=True))
cnn.add(LSTM(50, activation='relu'))
cnn.add(Dense(1))

#sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
#cnn.compile(loss='mse',    optimizer=sgd,
#              metrics=['mean_squared_error'])
cnn.compile(optimizer='adam', loss='mse')
# fit network
history = cnn.fit(train_X, train_y, epochs=10, batch_size=100, validation_data=(test_X, test_y), verbose=2)
# plot history
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()



yhat = cnn.predict(test_X)
test_X = test_X.reshape((test_X.shape[0], n_hours*n_features))
# invert scaling for forecast
inv_yhat = concatenate((yhat, test_X[:, -(n_features-1) :]), axis=1)
inv_yhat = scaler.inverse_transform(inv_yhat)
inv_yhat = inv_yhat[:,0]
# invert scaling for actual
test_y = test_y.reshape((len(test_y), 1))
inv_y = concatenate((test_y, test_X[:, -(n_features-1):]), axis=1)
inv_y = scaler.inverse_transform(inv_y)
inv_y = inv_y[:,0]
# calculate RMSE
rmse = sqrt(mean_squared_error(inv_y, inv_yhat))
print('Test RMSE: %.3f' % rmse)

from sklearn.metrics import r2_score
r2=r2_score(inv_y, inv_yhat)
print('Test RMSE: %.3f' % r2)

    
pyplot.plot(inv_yhat,label="prediction")
pyplot.plot(inv_y,label="real")
pyplot.legend()
pyplot.show()
