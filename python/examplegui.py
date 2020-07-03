

########################################################################3
##
## This will show how to use histogram and the time series utilities for
## the 2 sample comparison for tensors of size (C,H,W)
##
##
########################################################################3

from multiprocessing import Pool
import anomaly
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats 
import scipy.signal

from scipy.ndimage.filters import correlate, convolve


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


def from_tensor_to_series(A,B,tr=(1,0,2), plot = False):

    if tr != (0,1,2):
        A1 = A.transpose(tr)
        B1 = B.transpose(tr)
    else:
        A1 = A
        B1 = B

    if plot:
        plt.hist(A1.flatten(), bins=1000)
        plt.show()

        
    C,H,W = A1.shape 

    X = range(0,C)
    Y = []

    for x in X:
        t = [x]
        t.extend([ tw for tw in A1[x,:,:].flatten()])
        Y.append(t)
    for x in X:
        t = [x+len(X)]
        t.extend([ tw for tw in B1[x,:,:].flatten()])
        Y.append(t)

    return Y, C, H,W

def method(m, NP):
    if m ==1 and NP[4] == 1: return "POSET"
    elif m ==1 and NP[4] == 0: return "MST"
    elif m ==5 and NP[3] == 0: return "Kernel Linear"
    elif m ==5 and NP[3] == 1: return "Kernel Square"
    elif m==4 : return "Compression %d b" % NP[3]
    return "HMMM"

def f(X):
    thr = X.pop(0)
    verbose = X.pop(0)
    ## X  = [1,state,Y,NP,dim]
    met =  method(X[0],X[3])
    if verbose: print(met)
    F = anomaly.general_anomaly(X[0],X[1],X[2],X[3],X[4])
    Header = " item %f distance %f p-value %f "
    R = []

    if len(F)>1 :
        for fr in F[1]:
            for f in fr:
                R.append(f)
                if f[2] > thr:
                    if verbose:
                        print (method(X[0],X[3])+ (Header  %(f[0],f[1],f[2])))
                        print("_*_*_*_*_*_*_*_*_*___________")
    return [met, R ]          

def comparing_2N_sample_Pool(A,B, compression = True, thr = 0.95, tr = (1,0,2)):

    Verbose = False
    ## the series is is determined by H x (CW) as default
    Y,C,H,W = from_tensor_to_series(A,B,tr=tr)
    dim = H*W
    #import pdb; pdb.set_trace()
    Xs = []
    NP = [C,C, 1,0.2,1,1.0 ]
    state0 = None
    X = [thr,Verbose,1,state0,Y,NP,dim]
    Xs.append(X)
    state1 = None
    NP = [C,C, 1,0.2,0,1.0 ]
    X = [thr,Verbose,1,state1,Y,NP,dim]
    Xs.append(X)
    state2 = None
    NP = [C,C, 1, 0]
    X = [thr,Verbose,5,state2,Y,NP,dim]
    Xs.append(X)
    state3 = None
    NP = [C,C, 1, 1]
    X = [thr,Verbose,5,state3,Y,NP,dim]
    Xs.append(X)

    if compression:
        state4 = None
        NP = [C,C, 1, 32]
        X = [thr,Verbose,4,state4,Y,NP,dim]
        Xs.append(X)
    Rx = []
    with Pool(len(Xs)) as p:
        Rx.extend( p.map(f, Xs));

    for r in Rx:
        print(r)
    return Rx


def comparing_2N_sample(A,B, compression = True, thr = 0.95, tr = (1,0,2)):  
    ### Kolmogorov-Smirnov 2-N sample flat comparison
    ###
    R = []
    ks = scipy.stats.ks_2samp(A.flatten(), B.flatten())
    R.append(ks)
    print("KS 2NSample", ks)
    print("___________________")

    
    ## Y is a series composed by A and B.  The first parameter the
    ## method. Here we test them all. Howver, the compression will be
    ## the one taking longer.

    ## the series is is determined by H x (CW) as default
    Y,C,H,W = from_tensor_to_series(A,B,tr=tr)
    dim = H*W
    #import pdb; pdb.set_trace()


    NP = [C,C, 1,0.2,1,1.0 ]
    state = None
    X = [1,state,Y,NP,dim]
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
    NP = [C,C, 1,0.2,0,1.0 ]
    F = anomaly.general_anomaly(1,state,Y,NP,dim)
    if len(F)>1:
        for fr in F[1]:
            for f in fr:

                R.append(f)
                if f[2] > thr:
                    print ("MST " + (Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [C,C, 1, 0]
    F = anomaly.general_anomaly(5,state,Y,NP,dim)
    if len(F)>1:
        for fr in F[1]:
            for f in fr:
                R.append(f)
                if f[2] > thr:

                    print ("Kernel Linear " + (Header  %(f[0],f[1],f[2])))
                    print("_*_*_*_*_*_*_*_*_*___________")
    
    state = None
    NP = [C,C, 1, 1]
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
    NP = [C,C, 1, 100]
    
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


        comparing_2N_sample_Pool(A,B)  
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

        comparing_2N_sample_Pool(A,D)  
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
        comparing_2N_sample_Pool(A,D,False)  
    
    #import pdb;pdb.set_trace()
    kernel =  np.array([[2,2],[1,1]])

    K1 = A*1
    K2 = B*1

    for j in range(0,3):
        for i in range(0,C):
            k = 1/(j+1)
            K1[i,:,:] = scipy.signal.convolve2d(K1[i,:,:]*k, kernel, boundary='symm', mode='same')
            K2[i,:,:] = scipy.signal.convolve2d(K2[i,:,:], kernel, boundary='symm', mode='same')
            
        print(k, "_x_x_x_x_x_x_x_x_x_x_________")
        #import pdb; pdb.set_trace()    
        a = comparing_using_flat_hist(K1,K2)
        if a[1]> 0.95:
            print(a)
            print("_^_^_^_^_^_^_^_^_^_^_________")

        for i in range(0,C):
            a = comparing_using_flat_hist(K1[i,:,:],K2[i,:,:])
            if a[1]> 0.95:
                print("i->", i, a)
                print("_^_^_^_^_^_^_^_^_^_^_________")
        
        ## Can we capture the difference if we are using 2N-Sample
        ## comparison ?
        comparing_2N_sample_Pool(K1,K2,True)  
    
    # now introducing kernel quantized differently
    kernelA =  np.array([[2.2,2.1],[1.2,1.1]])
    kernelB =  np.array([[2.1,2.2],[1.1,1.2]])

    for i in range(0,C):
        K1[i,:,:] = scipy.signal.convolve2d(A[i,:,:], kernelA, boundary='symm', mode='same')
        K2[i,:,:] = scipy.signal.convolve2d(A[i,:,:], kernelB, boundary='symm', mode='same')

        
    print("_x_x_x_x_x_x_x_x_x_x_________")
    
    a = comparing_using_flat_hist(K1,K2)
    if a[1]> 0.95:
        print(a)
        print("_^_^_^_^_^_^_^_^_^_^_________")
        
    for i in range(0,C):
        a = comparing_using_flat_hist(K1[i,:,:],K2[i,:,:])
        if a[1]> 0.95:
            print("i->", i, a)
            print("_^_^_^_^_^_^_^_^_^_^_________")
        
    comparing_2N_sample_Pool(K1,K2)  

    ## now we go fancy: we create an comparison with [y = f1(x),x ] vs
    ## [y=f2(x),x] we create a vector output + inputs and compare this
    ## sequence.
    
    
    templea = np.ndarray((C*(W-1)*(H-1),5))*0.0
    templeb = np.ndarray((C*(W-1)*(H-1),5))*0.0
    l = -1
    for i in range(0,C):
        K1[i,:,:] = correlate(A[i,:,:], kernelA, mode='constant', cval=0.0)
        K2[i,:,:] = correlate(A[i,:,:], kernelB, mode='constant',cval=0.0)
        for h in range(0,H-1):
            for w in range(0,W-1):
                l+=1
                ## building [f(x), x]
                
                q = [K1[i,h,w]]
                q.extend([ tw for tw in A[i,h:h+2,w:w+2].flatten()])
                templea[l,:] = q
                q = [K2[i,h,w]]
                q.extend([ tw for tw in A[i,h:h+2,w:w+2].flatten()])
                templeb[l,:] = q
    
    K1 = templea.reshape((C*(W-1)*(H-1),5,1))
    K2 = templeb.reshape((C*(W-1)*(H-1),5,1))
    print(K1.shape)
    print("_x_x_x_x_x_x_x_x_x_x_________")
    #import pdb; pdb.set_trace()    
    a = comparing_using_flat_hist(K1,K2)
    if a[1]> 0.95:
        print(a)
        print("_^_^_^_^_^_^_^_^_^_^_________")
        
    SAMPLE = C*(W-1)*(H-1)> 15000

    if SAMPLE:
        sample = 10000
    else:
        sample = C*(W-1)*(H-1)
    
    ## Can we capture the difference if we are using 2N-Sample
    ## comparison ?
    if SAMPLE:
        comparing_2N_sample_Pool(K1[0:sample,:,:],
                                 K2[0:sample,:,:],
                                 compression = False,
                                 thr=0.95,
                                 tr = (0,1,2)
        )  
    else:
        comparing_2N_sample_Pool(K1,
                                 K2,
                                 compression = True,
                                 thr=0.95,
                                 tr = (0,1,2)
        )
    
    
