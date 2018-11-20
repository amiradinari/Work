import os
import numpy as np
import cv2
import math
from torch.optim import SGD
from pyquaternion import Quaternion
import multiprocessing as mp

import time
path='/cise/research51/cvgmi_04/amira/results_filter'
os.chdir(path)
'/cise/homes/dinari/Desktop'
def Quat_RGB(source):
   v=np.zeros((1,4))[0]
   q0=Quaternion(1,source[0],source[1],source[2])
   v[0]=1.0
   v[1]=round(q0[1])
   v[2]=round(q0[2])
   v[3]=round(q0[3])
   return(v)
def DISTAN_RGB(q0,q1):
     return(Quaternion.sym_distance(q0, q1))
def distance(x, y,i,j):
    return np.sqrt((x-i)**2 + (y-j)**2)
def L2_distance(u,i):
    return np.sqrt((u[0]-i[0])**2 + (u[1]-i[1])**2+(u[2]-i[2])**2+(u[3]-i[3]**2))
def log_Euclid(q0,q1):
    res=Quaternion.log(q0)-Quaternion.log(q1)
    return(res.norm)

def gaussian (x, sigma):
 return  math.exp(- (x ** 2) / (2 * sigma ** 2))
def alpha_y(source,sigma_s):
     diameter=(2*sigma_s-1)*2
     hl=diameter/2
     x0=0
     y0=0
     i=0
     
    
     while i < diameter:
        j = 0
        while j < diameter:
            neighbour_x = x0 - (hl - i)
            neighbour_y = y0 - (hl - j)
            if neighbour_x >= len(source):
                neighbour_x -= len(source)
            if neighbour_y >= len(source[0]):
                neighbour_y -= len(source[0])
            alpha_y = gaussian(distance(neighbour_x, neighbour_y, x0, y0), sigma_s)
           
            j=j+1
        i=i+1 
     
     return(alpha_y)
  
def Quaternion_Matrix(source):
    quat_image=np.zeros((len(source),len(source[0]),4))
    for i in range(0,len(source)):
        for j in range (0,len(source[0])):
            quat_image[i,j]=Quat_RGB(source[i,j])
    return(quat_image)
 
def alpha(Q,diameter,sigma_i,sigma_s,x,y): 
     i_filtered = 0
     Wp=0
     if diameter % 2 == 0:
      print("diameter should be odd")
     else:
      hl = int(round(diameter/2))-1
     i=0
     while i < diameter  :
       j = 0
       while j < diameter :
           neighbour_x =x - (hl - i)
           neighbour_y = y - (hl - j)
           if neighbour_x >= len(Q):
              neighbour_x -= len(Q)
           if neighbour_y >= len(Q[0]):
              neighbour_y -= len(Q[0])
           if neighbour_x<0:
                neighbour_x += len(Q)
           if neighbour_y<0:
                neighbour_y += len(Q[0])
           alpha_x = gaussian(DISTAN_RGB(Quaternion(Q[x,y]),Quaternion(Q[round(neighbour_x),round(neighbour_y)])), sigma_i)
           alpha_y = gaussian(distance(neighbour_x, neighbour_y, x, y), sigma_s)
           alpha=alpha_x*alpha_y
           i_filtered += Q[round(neighbour_x)][round(neighbour_y)] * alpha
           Wp += alpha
           j=j+1
       i=i+1 
     i_filtered = i_filtered / Wp
     return(i_filtered)
def alpha_log_Eucl(source,sigma_i,sigma_s,x,y,filtered_Quaternion): 
     i_filtered = 0
     Wp=0
     Q=Quaternion_Matrix(source)
     diameter=(2*sigma_s-1) *2
     hl = (2*sigma_s-1)
     i=0
     while i < diameter  :
       j = 0
       while j < diameter :
           neighbour_x =x - (hl - i)
           neighbour_y = y - (hl - j)
           if neighbour_x >= len(source):
              neighbour_x -= len(source)
           if neighbour_y >= len(source[0]):
              neighbour_y -= len(source[0])
           alpha_x = gaussian(log_Euclid(Quaternion(Q[x,y]),Quaternion(Q[round(neighbour_x),round(neighbour_y)])), sigma_i)
           alpha_y = gaussian(distance(neighbour_x, neighbour_y, x, y), sigma_s)
           alpha=alpha_x*alpha_y
           i_filtered += Q[round(neighbour_x)][round(neighbour_y)] * alpha
           Wp += alpha
           j=j+1
       i=i+1 
     i_filtered = i_filtered / Wp
     filtered_Quaternion[x][y] = i_filtered       
def Apply_BF(source,diameter, sigma_i, sigma_s):
    Q=Quaternion_Matrix(source)
    filtered_Quaternion = np.zeros(Q.shape)
    i = 0
    while i < len(source):
        j = 0
        while j < len(source[0]):
            filtered_Quaternion[i,j]= alpha(Q,diameter,sigma_i,sigma_s,i,j)
            j += 1
        i += 1
    return filtered_Quaternion
def Apply_BF_log_Eucl(source, sigma_i, sigma_s):
    Q=Quaternion_Matrix(source)
    filtered_Quaternion = np.zeros(Q.shape)
    i = 0
    while i < len(source):
        j = 0
        while j < len(source[0]):
            alpha_log_Eucl(source,sigma_i,sigma_s,i,j,filtered_Quaternion)
            j += 1
        i += 1
    return filtered_Quaternion  
def Inductive_Mean_Quat(Q, x, y, diameter, sigma_i, sigma_s):
    if diameter % 2 == 0:
      print("diameter should be odd")
    else:
      hl = int(round(diameter/2))-1
    i_filtered_ind =Quaternion(Q[x-hl,y-hl])
#    i_filtered=0
    Wp = 0
    i = 0
    while i < diameter:
        j = 0
        while j < diameter:
            neighbour_x = x - (hl - i)
            neighbour_y = y - (hl - j)
            if neighbour_x >= len(Q):
                neighbour_x -= len(Q)
            if neighbour_y >= len(Q[0]):
                neighbour_y -= len(Q[0])
            if neighbour_x<0:
                neighbour_x += len(Q)
            if neighbour_y<0:
                neighbour_y += len(Q[0])
            gi =gaussian(DISTAN_RGB(Quaternion(Q[x,y]),Quaternion(Q[round(neighbour_x),round(neighbour_y)])), sigma_i)
            gs =gaussian(distance(neighbour_x, neighbour_y, x, y), sigma_s)
            w = gi * gs
            Wp +=w
            if Wp==0 and w==0:
                  Wp=1
            if i!=0 and j!=0 :
#               i_filtered_ind=i_filtered_ind**(1/2)*Quaternion.exp((w/Wp)*Quaternion.log(i_filtered_ind**(-1/2)*Quaternion(Q[round(neighbour_x),round(neighbour_y)])*i_filtered_ind**(-1/2)))*i_filtered_ind**(1/2)
                
                i_filtered_ind=i_filtered_ind**(1/2)*(i_filtered_ind**(-1/2)*Quaternion(Q[round(neighbour_x),round(neighbour_y)])*i_filtered_ind**(-1/2))**(w/Wp)*i_filtered_ind**(1/2)
            j += 1
        i += 1
    i_filtered_ind = [i_filtered_ind[1],i_filtered_ind[2],i_filtered_ind[3]]
    return(i_filtered_ind)
    
    
def Inductive_Mean_Quat_log_Euclidean(Q, x, y, diameter, sigma_i, sigma_s):
    if diameter % 2 == 0:
      print("diameter should be odd")
    else:
      hl = int(round(diameter/2))-1
    i_filtered_ind =Quaternion(Q[x-hl,y-hl])
#    i_filtered=0
    Wp = 0
    i = 0
    while i < diameter:
        j = 0
        while j < diameter:
            neighbour_x = x - (hl - i)
            neighbour_y = y - (hl - j)
            if neighbour_x >= len(Q):
                neighbour_x -= len(Q)
            if neighbour_y >= len(Q[0]):
                neighbour_y -= len(Q[0])
            if neighbour_x<0:
                neighbour_x += len(Q)
            if neighbour_y<0:
                neighbour_y += len(Q[0])
            gi =gaussian(log_Euclid(Quaternion(Q[x,y]),Quaternion(Q[round(neighbour_x),round(neighbour_y)])), sigma_i)
            gs =gaussian(distance(neighbour_x, neighbour_y, x, y), sigma_s)
            w = gi * gs
            Wp +=w
            if Wp==0 and w==0:
                  Wp=1
            if i!=0 and j!=0 :
#               i_filtered_ind=i_filtered_ind**(1/2)*Quaternion.exp((w/Wp)*Quaternion.log(i_filtered_ind**(-1/2)*Quaternion(Q[round(neighbour_x),round(neighbour_y)])*i_filtered_ind**(-1/2)))*i_filtered_ind**(1/2)
                i_filtered_ind=i_filtered_ind**(1/2)*(i_filtered_ind**(-1/2)*Quaternion(Q[round(neighbour_x),round(neighbour_y)])*i_filtered_ind**(-1/2))**(1-w/Wp)*i_filtered_ind**(1/2)
            j += 1
        i += 1
    i_filtered_ind = [i_filtered_ind[1],i_filtered_ind[2],i_filtered_ind[3]]
    return(i_filtered_ind)
    
def Apply_Inductive_mean_Quat(source, filter_diameter, sigma_i, sigma_s):
    Q=Quaternion_Matrix(source)
    filtered_image = np.zeros(source.shape)
    i = 0
    while i < len(source):
        j = 0
        while j < len(source[0]):
            filtered_image[i,j]=Inductive_Mean_Quat(Q, i, j, filter_diameter, sigma_i, sigma_s)
            print(i,j)
            j += 1
        i += 1
    return filtered_image  

def Apply_Inductive_mean_Quat_log_Eucl(source, filter_diameter, sigma_i, sigma_s):
    Q=Quaternion_Matrix(source)
    filtered_image = np.zeros(source.shape)
    i = 0
    while i < len(source):
        j = 0
        while j < len(source[0]):
            filtered_image[i,j]=Inductive_Mean_Quat_log_Euclidean(Q, i, j, filter_diameter, sigma_i, sigma_s)
            print(i,j)
            j += 1
        i += 1
    return filtered_image   
def Geodisc_path(q0,q1):
   for i in range(0,len(alpha)):
       p=sum(Quaternion.sym_log_map(q0,q1))
       p1=Quaternion.sym_exp_map(p)
       return(p1)
def Quat_Mean(source,sigma_i,sigma_s):
#  epsilon=0.01
  diameter=(2*sigma_s-1)
#  N=diameter*diameter
  Q=Quaternion_Matrix(source)
  A=np.zeros(Q.shape)
  k=0
  f=np.zeros(Q.shape)
  for x in range(0,len(Q)):
      for y in range (0,len(Q[0])):
          neighbour_x=x
          neighbour_y=y
          i=0
          while i < diameter and neighbour_x < diameter+1:
             j = 0
             while j < diameter and neighbour_y < diameter+1 :
                neighbour_x = x +i
                neighbour_y = y +i
                if neighbour_x >= len(source):
                   neighbour_x -= len(source)
                if neighbour_y >= len(source[0]):
                   neighbour_y -= len(source[0])
                A[x,y]=alpha(source,sigma_i,sigma_s,x,y)*math.pow(DISTAN_RGB(Quaternion(Q[x,y]),Quaternion(Q[round(neighbour_x),round(neighbour_y)])),2)
                f[x,y]=Geodisc_path(Quaternion(Q[x,y],Quaternion(A[neighbour_x,neighbour_x])))
                j=j+1
             i=i+1
          k=k+1  
            
  return(f)
def scale(A):
    out=np.zeros((len(A),len(A[0]),3))
    min=np.min(A)
    max=np.max(A)
    for i in range(0,len(A)):
        for j in range (0,len(A[0])):
         out[i,j]=((A[i,j]-min)/(max-min))*255
         out[i,j]=np.round(out[i,j])
    return(out)  
  #results
image=cv2.imread('before filter.png')
start_time=time.clock

Syntahtique=Process_image(Syntahtique)
N=noisy(Syntahtique,10)
pool = mp.Pool(processes=8)
results = [pool.apply(Apply_Inductive_mean_Quat_log_Eucl, args=(N,7,0.2,4,)) ]
output = [p.get() for p in results]
print(time.clock-start_time,"seconds")
image=cv2.imread('color_before.png')    
Q=Apply_BF(image,4.0,4.0)
filtred=Q[:,:,1:4]
cv2.imwrite("Quat.png", filtred)  

