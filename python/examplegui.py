

########################################################################3
##
## This will show how to use histogram and the time series utilities for
## the 2 sample comparison for tensors of size (C,H,W)
##
##
########################################################################3


import anomaly
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats 



C = 3
H = 500
W = 100

A = np.ndarray((C,H,W), buffer=np.array([np.random.rand(1) for i in range(0,C*H*W)]))
B = np.ndarray((C,H,W), buffer=np.array([np.random.rand(1) for i in range(0,C*H*W)]))


def comparing(A,B):  
    ### Kolmogorov-Smirnov 2-N sample comparison
    ###
    R = []
    ks = scipy.stats.ks_2samp(A.flatten(), B.flatten())
    R.append(ks)
    print(ks)
    print("___________________")

    ###  Histogram comparison 1000 bins the comparison
    ###  the bin is a string: we assume a perfect match
    ###  with the bin.
     
    bins = [ i for i in range(0,1000)]
    
    ha,bina = np.histogram(A.flatten(), bins=1000)
    hb,binb = np.histogram(B.flatten(), bins=1000)
    
    f1 = {}
    f2 = {}
    for i in range(0,len(bins)):
        f1[str(i)] = ha[i] if i <len(ha) else 0
        f2[str(i)] = hb[i] if i <len(hb) else 0


    a = anomaly.histogram_comp(list(f1.items()),list(f1.items()))
    print(a)
    R.append(a)
    print("_^_^_^_^_^_^_^_^_^_^_________")
    
    ### How to build a series using the Height as reference
    ### the series has H entries and C*W point (dimensions)
    
    X = range(0,H)
    Y = []
    dim = C*W
    
    for x in X:
        t = [x]
        for c in range(0,C):
            for w in range(0,W):
                t.append(A[c,x,w])
        Y.append(t)
    for x in X:
        t = [x+len(X)]
        for c in range(0,C):
            for w in range(0,W):
                t.append(B[c,x,w])
        Y.append(t)
    
        
    NP = [H,H, 1,0.2,1,1.0 ]
    state = None
    F = anomaly.general_anomaly(1,state,Y,NP,dim)
    if len(F)>1:
        for f in F[1]:
            R.append(f)
            print ("POSET RF item %f distance %f p-value %f (larger significant distace)" %(f[0],f[1],f[2]))
    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [H,H, 1,0.2,0,1.0 ]
    F = anomaly.general_anomaly(1,state,Y,NP,dim)
    if len(F)>1:
        for f in F[1]:
            R.append(f)
            print ("MST RF item %f distance %f p-value %f (larger significant distace)" %(f[0],f[1],f[2]))
    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [H,H, 1, 0]
    F = anomaly.general_anomaly(5,state,Y,NP,dim)
    if len(F)>1:
        for f in F[1]:
            R.append(f)
            print ("Kernel Linear RF item %f distance %f p-value %f (larger significant distace)" %(f[0],f[1],f[2]))
    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [H,H, 1, 1]
    F = anomaly.general_anomaly(5,state,Y,NP,dim)
    if len(F)>1:
        for f in F[1]:
            R.append(f)
            print ("Kernel Square RF item %f distance %f p-value %f (larger significant distace)" %(f[0],f[1],f[2]))
    print("_*_*_*_*_*_*_*_*_*___________")
    
    
    state = None
    NP = [H,H, 1, 100]
    
    F = anomaly.general_anomaly(4,state,Y,NP,dim)
    if len(F)>1:
        for f in F[1]:
            R.append(f)
            print ("Compression 100 Bootstrap RF item %f distance %f p-value %f (larger significant distace)" %(f[0],f[1],f[2]))
    print("_*_*_*_*_*_*_*_*_*___________")
    
    
    
if __name__ == "__main__":

    ## this should be the same
    comparing(A,B)    

    ## this should be the different
    comparing(A,B*A)    

    ## this should be the different
    comparing(A,B+A)    
    
    
    
    
