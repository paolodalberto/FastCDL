

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
import scipy.signal



C = 3
H = 500
W = 100

A = np.ndarray((C,H,W), buffer=np.random.rand(C*H*W))
B = np.ndarray((C,H,W), buffer=np.random.rand(C*H*W))

def comparing_using_flat_hist(A,B, plot = False):
    ###  Histogram comparison 1000 bins
    ###  the bin is a string: we assume a perfect match
    ###  with the bin.
     
    ## we create a range and thus a 1000 bins
    m = min(min(A.flatten()), min(B.flatten()))
    M = max(max(A.flatten()), max(B.flatten()))
    bins = []
    i = m
    while i <M:
        bins.append(i)
        i += (M-m)/1000

    
    ha,bina = np.histogram(A.flatten(), bins=bins)
    hb,binb = np.histogram(B.flatten(), bins=bins)

    if plot:
        print(np.mean(A.flatten()), np.var(A.flatten()))
        print(np.mean(B.flatten()), np.var(B.flatten()))
    
        plt.plot(bins[1:],ha)
        plt.plot(binb[1:],hb)
        plt.show()
        
        
    f1 = {}
    f2 = {}
    for i in range(0,len(bins)):
        f1[str(i)] = ha[i] if i <len(ha) else 0
        f2[str(i)] = hb[i] if i <len(hb) else 0


    a = anomaly.histogram_comp(list(f1.items()),list(f2.items()))
    del f1
    del f2
    return a



def comparing_2N_sample(A,B, compression = True, thr = 0.95):  
    ### Kolmogorov-Smirnov 2-N sample flat comparison
    ###
    R = []
    ks = scipy.stats.ks_2samp(A.flatten(), B.flatten())
    R.append(ks)
    print("KS 2NSample", ks)
    print("___________________")

    C,H,W = A.shape 
    ### How to build a series using the Height as reference the series
    ### has H entries and C*W point (dimensions) This is realistic in
    ### the sense that we can compute the output tensor in section and
    ### the computation could be the reason of the difference.

    ### H becomes the number of samples: more samples more
    ### representative the distribution.
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

    
    ## Y is a series composed by A and B.  The first parameter the
    ## method. Here we test them all. Howver, the compression will be
    ## the one taking longer.

    #    import pdb; pdb.set_trace()
    NP = [H,H, 1,0.2,1,1.0 ]
    state = None
    F = anomaly.general_anomaly(1,state,Y,NP,dim)
    Header = " item %f distance %f p-value %f "
    if len(F)>1 :
        for fr in F[1]:
            for f in fr:
                R.append(f)
                if f[2] > thr:
                    print ("POSET "+ (Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [H,H, 1,0.2,0,1.0 ]
    F = anomaly.general_anomaly(1,state,Y,NP,dim)
    if len(F)>1:
        for fr in F[1]:
            for f in fr:

                R.append(f)
                if f[2] > thr:
                    print ("MST " + (Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [H,H, 1, 0]
    F = anomaly.general_anomaly(5,state,Y,NP,dim)
    if len(F)>1:
        for fr in F[1]:
            for f in fr:
                R.append(f)
                if f[2] > thr:

                    print ("Kernel Linear " + (Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [H,H, 1, 1]
    F = anomaly.general_anomaly(5,state,Y,NP,dim)
    if len(F)>1:
        for fr in F[1]:
            for f in fr:

                R.append(f)
                if f[2] > thr:
                    print ("Kernel Square " +(Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    

    if not compression: return R
    state = None
    NP = [H,H, 1, 100]
    
    F = anomaly.general_anomaly(4,state,Y,NP,dim)
    if len(F)>1:
        for fr in F[1]:
            for f in fr:

                R.append(f)
                if f[2] > thr:
                    print ("Compr 100 Bootstrap " + (Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    
    return R
    
if __name__ == "__main__":


    if True:
        print("A vs B _v_v_v_v_v_v_v_v_v_v_________")
        ## this should be the same 
        a = comparing_using_flat_hist(A,B)
        if a[1]> 0.95:
            print(a)
        
        for i in range(0,C):
            a = comparing_using_flat_hist(A[i,:,:],B[i,:,:])
            if a[1]> 0.95:
                print("i ->", i, a)
                print("_^_^_^_^_^_^_^_^_^_^_________")


        comparing_2N_sample(A,B,False)  
        print("A vs A*B _v_v_v_v_v_v_v_v_v_v_________")
        ## this should be the different
        D = A*B 
        a = comparing_using_flat_hist(A,D)
        if a[1]> 0.95:
            print(a)
        
        for i in range(0,C):
            a = comparing_using_flat_hist(A[i,:,:],D[i,:,:])
            if a[1]> 0.95:
                print("i -> ", i, a)
                print("_^_^_^_^_^_^_^_^_^_^_________")

        comparing_2N_sample(A,D,False)  
        print("B[1] diff A[1] _v_v_v_v_v_v_v_v_v_v_________")
        ## this should be different because one channel is different. A
        ## flat comparison likely fails.
        D = A*1
        D[1,:,:] = D[1,:,:]*0.8
        a = comparing_using_flat_hist(A,D)
        if a[1]> 0.95:
            print(a)
        
        for i in range(0,C):
            a = comparing_using_flat_hist(A[i,:,:],D[i,:,:])
            if a[1]> 0.95:
                print("i->", i, a)
                print("_^_^_^_^_^_^_^_^_^_^_________")
            
        ## Can we capture the difference if we are using 2N-Sample
        ## comparison ?
        comparing_2N_sample(A,D,False)  
    
    #import pdb;pdb.set_trace()
    kernel =  np.array([[2,2],[1,1]])

    K = A*1
    H = B*1

    for j in range(0,2):
        for i in range(0,C):
            k = 1/(j+1)
            K[i,:,:] = scipy.signal.convolve2d(K[i,:,:]*k, kernel, boundary='symm', mode='same')
            H[i,:,:] = scipy.signal.convolve2d(H[i,:,:], kernel, boundary='symm', mode='same')
            
        print(k, "_x_x_x_x_x_x_x_x_x_x_________")
        #import pdb; pdb.set_trace()    
        a = comparing_using_flat_hist(H,K)
        if a[1]> 0.95:
            print(a)
            print("_^_^_^_^_^_^_^_^_^_^_________")

        for i in range(0,C):
            a = comparing_using_flat_hist(H[i,:,:],K[i,:,:])
            if a[1]> 0.95:
                print("i", i, a)
                print("_^_^_^_^_^_^_^_^_^_^_________")
        
        ## Can we capture the difference if we are using 2N-Sample
        ## comparison ?
        comparing_2N_sample(H,K,True)  
    
