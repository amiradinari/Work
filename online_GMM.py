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
from scipy import stats
path="C:/Users/adineri/Desktop/GMM_test.txt"
df = pd.read_csv(path,sep=", ",header = None)

def kendalltau_costum(x, cov_0, initial_lexsort=None, nan_policy='propagate', method='auto'):
    """
    Calculate Kendall's tau, a correlation measure for ordinal data.
    Kendall's tau is a measure of the correspondence between two rankings.
    Values close to 1 indicate strong agreement, values close to -1 indicate
    strong disagreement.  This is the 1945 "tau-b" version of Kendall's
    tau [2]_, which can account for ties and which reduces to the 1938 "tau-a"
    version [1]_ in absence of ties.
    Parameters
    ----------
    x, y : array_like
        Arrays of rankings, of the same shape. If arrays are not 1-D, they will
        be flattened to 1-D.
    initial_lexsort : bool, optional
        Unused (deprecated).
    nan_policy : {'propagate', 'raise', 'omit'}, optional
        Defines how to handle when input contains nan. 'propagate' returns nan,
        'raise' throws an error, 'omit' performs the calculations ignoring nan
        values. Default is 'propagate'. Note that if the input contains nan
        'omit' delegates to mstats_basic.kendalltau(), which has a different
        implementation.
    method: {'auto', 'asymptotic', 'exact'}, optional
        Defines which method is used to calculate the p-value [5]_.
        'asymptotic' uses a normal approximation valid for large samples.
        'exact' computes the exact p-value, but can only be used if no ties
        are present. 'auto' is the default and selects the appropriate
        method based on a trade-off between speed and accuracy.
    Returns
    -------
    correlation : float
       The tau statistic.
    pvalue : float
       The two-sided p-value for a hypothesis test whose null hypothesis is
       an absence of association, tau = 0.
    See also
    --------
    spearmanr : Calculates a Spearman rank-order correlation coefficient.
    theilslopes : Computes the Theil-Sen estimator for a set of points (x, y).
    weightedtau : Computes a weighted version of Kendall's tau.
    Notes
    -----
    The definition of Kendall's tau that is used is [2]_::
      tau = (P - Q) / sqrt((P + Q + T) * (P + Q + U))
    where P is the number of concordant pairs, Q the number of discordant
    pairs, T the number of ties only in `x`, and U the number of ties only in
    `y`.  If a tie occurs for the same pair in both `x` and `y`, it is not
    added to either T or U.
    References
    ----------
    .. [1] Maurice G. Kendall, "A New Measure of Rank Correlation", Biometrika
           Vol. 30, No. 1/2, pp. 81-93, 1938.
    .. [2] Maurice G. Kendall, "The treatment of ties in ranking problems",
           Biometrika Vol. 33, No. 3, pp. 239-251. 1945.
    .. [3] Gottfried E. Noether, "Elements of Nonparametric Statistics", John
           Wiley & Sons, 1967.
    .. [4] Peter M. Fenwick, "A new data structure for cumulative frequency
           tables", Software: Practice and Experience, Vol. 24, No. 3,
           pp. 327-336, 1994.
    .. [5] Maurice G. Kendall, "Rank Correlation Methods" (4th Edition),
           Charles Griffin & Co., 1970.
    Examples
    --------
    >>> from scipy import stats
    >>> x1 = [12, 2, 1, 12, 2]
    >>> x2 = [1, 4, 7, 1, 0]
    >>> tau, p_value = stats.kendalltau(x1, x2)
    >>> tau
    -0.47140452079103173
    >>> p_value
    0.2827454599327748
    """
    x = np.asarray(x).ravel()
    cov_0 = np.asarray(cov_0).ravel()

    if x.shape[1]* x.shape[1] != cov_0.shape[0]* cov_0.shape[1]:
        raise ValueError("All inputs to `kendalltau` must be of the same size, "
                        "found x-size %s and y-size %s" % (x.size, y.size))
#    elif not x.size or not y.size:
#        return KendalltauResult(np.nan, np.nan)  # Return NaN if arrays are empty

    # check both x and y
    cnx, npx = _contains_nan(x, nan_policy)
    cny, npy = _contains_nan(cov_0, nan_policy)
    contains_nan = cnx or cny
    if npx == 'omit' or npy == 'omit':
        nan_policy = 'omit'

    if contains_nan and nan_policy == 'propagate':
        return KendalltauResult(np.nan, np.nan)

    elif contains_nan and nan_policy == 'omit':
        x = ma.masked_invalid(x)
        y = ma.masked_invalid(y)
        return mstats_basic.kendalltau(x, y, method=method)

    if initial_lexsort is not None:  # deprecate to drop!
        warnings.warn('"initial_lexsort" is gone!')
        
        
    tri_lower_diag = np.linalg.cholesky(cov_0)  
    yi=np.matmul(linalg.inv(tri_lower_diag),x)
    S_y=np.cov(yi)
    I=np.identity(x.shape[0])
    W=(1/x.shape[0])(np.matmul(S_y-I,S_y-I)).trace()-(x.shape[1]/x.shape[0])(S_y.trace()/x.shape[1])*(S_y.trace()/x.shape[1])+(x.shape[1]/x.shape[0])
    test=scipy.stats.chisquare(((((x.shape[0]*W-x.shape[1])*x.shape[1])/2)-x.shape[1]),ddof=(x.shape[1](x.shape[1]+1))/2)
    chi,p_value=test
    return(p_value)
    
def mean_hist(X_hist):
     lowest_bic = np.infty
     bic = []
     n_components_range = range(1, 10)
     cv_types =  'full'
     for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
            gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_types)
            gmm.fit(X_hist)
            bic.append(gmm.bic(X_hist))
            if bic[-1] < lowest_bic:
                lowest_bic = bic[-1]
                best_gmm = gmm
     means=best_gmm._get_parameters()[1]
     return(means)
  
def cov_hist(X_hist):
     lowest_bic = np.infty
     bic = []
     n_components_range = range(1, 10)
     cv_types = 'full'
     for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
             gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_types)
             gmm.fit(X_hist)
             bic.append(gmm.bic(X_hist))
             if bic[-1] < lowest_bic:
              lowest_bic = bic[-1]
              best_gmm = gmm
     cov=best_gmm._get_parameters()[2]
     return(cov)
  
def estimate_ka(X):

     lowest_bic = np.infty
     bic = []
     out_comp=0
     n_components_range = range(1,10)
     cv_types = 'full'
     
     for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
          gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_types)
          gmm.fit(X)
          bic.append(gmm.bic(X))
          if bic[-1] < lowest_bic:
              lowest_bic = bic[-1]
              out_comp=n_components
     return(out_comp )  
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
             if (kendalltau_costum(labled_stream(X_stream)[i],cov_hist(X_hist)[j])>0.05):
                T2=spm1d.stats.hotellings(mean_hist(X_hist)[j],labled_stream(X_stream)[i]) 
                if (T2.inference(0.05).h0reject==False):
                    index_ka=i
                    index_kg=j
                    ### we merge both components
                    
       return([index_ka,index_kg])
       
class Online_GMM:
    def __init__(self,X_hist,X_stream):
        self.Historical_data=X_hist
        self.streaming_data=X_stream
####PDF ESTIMATING #####
##Applied on the historical data 
    def gaussPDF(data):
     g = mixture.GaussianMixture(n_components=1).fit(data)
     logProb = g.score_samples(data)
     prob = np.exp( logProb )
     return prob
    
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
###gives updates historical data  
   
                    
    def updated(self):
      X_hist,X_stream=self.Historical_data,self.streaming_data
      
      [i,j]=test(X_hist,X_stream)
      frames = [labled_stream(X_hist)[j],labled_stream(X_stream)[i]]
      result=pd.concat(frames)
      self.remained_X_stream=labled_stream(X_stream).pop(i)
      self.remained_X_hist=labled_stream(X_hist).pop(j)    
      self.updated_X_hist=self.remained_X_hist.append(result)
      self.updated_X_hist=self.updated_X_hist.append(pd.DataFrame(self.remained_X_stream))
      return(self.updated_X_hist)


    
