# -*- coding: utf-8 -*-
"""
Created on Fri Jan 11 08:31:25 2019

@author: adineri
"""
import numpy as np
import math
import sys
from matplotlib.path import Path
import matplotlib.patches as patches
import scipy.linalg as lin
import matplotlib.pyplot as plt
from sklearn import mixture
import pandas as pd
import scipy
import spm1d
path="C:/Users/adineri/Desktop/GMM_test.txt"
df = pd.read_csv(path,sep=", ",header = None)

      
####PDF ESTIMATING #####
##Applied on the historical data 
def gaussPDF(data):
   g = mixture.GaussianMixture(n_components=1).fit(data)
   logProb = g.score_samples(data)
   prob = np.exp( logProb )
   return prob

def mean_hist(X_hist):
  lowest_bic = np.infty
  bic = []
  n_components_range = range(1, 10)
  cv_types = ['spherical', 'tied', 'diag', 'full']
  for cv_type in cv_types:
      for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
          gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_type)
          gmm.fit(X_hist)
          bic.append(gmm.bic(X_hist))
          if bic[-1] < lowest_bic:
              lowest_bic = bic[-1]
              best_gmm = gmm
  means=best_gmm.means_
  return(means)
  
def cov_hist(X_hist):
  lowest_bic = np.infty
  bic = []
  n_components_range = range(1, 10)
  cv_types = ['spherical', 'tied', 'diag', 'full']
  for cv_type in cv_types:
      for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
          gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_type)
          gmm.fit(X_hist)
          bic.append(gmm.bic(X_hist))
          if bic[-1] < lowest_bic:
              lowest_bic = bic[-1]
              best_gmm = gmm
  cov=best_gmm.covariances__
  return(cov)
  
####ESTIMATE ka WITH BIC for the streaming data    
def estimate_ka(X):

  lowest_bic = np.infty
  bic = []
  out_comp=0
  n_components_range = range(1, 7)
  cv_types = ['spherical', 'tied', 'diag', 'full']
  for cv_type in cv_types:
      for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
          gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_type)
          gmm.fit(X)
          bic.append(gmm.bic(X))
          if bic[-1] < lowest_bic:
              lowest_bic = bic[-1]
              out_comp=n_components
  return(out_comp )  
  
###estimate the posteriorate probablity 
def estimate_pos_prob(X):

  lowest_bic = np.infty
  bic = []
  n_components_range = range(1, 10)
  cv_types = ['spherical', 'tied', 'diag', 'full']
  for cv_type in cv_types:
      for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
          gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_type)
          gmm.fit(X)
          bic.append(gmm.bic(X))
          if bic[-1] < lowest_bic:
              lowest_bic = bic[-1]
              best_gmm = gmm
              
  p=best_gmm.predict_proba(X)
  return(p )
 



def labled_stream(X):
  ka=estimate_ka(X)
  gmm = mixture.GaussianMixture(n_components=ka).fit(X)
  labels = gmm.predict(X)
  list_data=[]
  for i in range(0,ka):
      index=[x for x in range(0,labels.shape[0]) if labels[x]==i] 
      list_data.append(X.iloc[index,:])
  
    
  return(list_data ) 
  
def test(X_hist,X_stream):
    ka=estimate_ka(X_stream)
    kg=estimate_ka(X_hist)
    for i in range(0,ka):
        for j in range(0,kg):
            if (scipy.stats.kendalltau(cov_hist(X_hist)[j],cov_hist(labled_stream(X_stream[i])))>0.7):
                T2=spm1d.stats.hotellings2(cov_hist(X_hist)[j],cov_hist(labled_stream(X_stream[i]))) 
                if (T2.inference(0.05).h0reject==False):
                     T2=0
                     
 