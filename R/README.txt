# How to build the R interface 

I like R, the combination of a simple syntax and nice graphical
interface allows to create very powerful and graphical tools.

## clean up the C library First

```
cd ../
make clean
```

# We build the R Module: R needs -DRDEF 

I noticed that to have a complete installation is better to have a
full path: set the HOMEDIR to the C library path FastCDL

```
cd R/CDL/src
make R

cd ../../
R CMD build CDL
sudo R CMD INSTALL CDL_0.1.tar.gz
```

In practice, you do not need to have sudo privileges. But I like to
have a clean installation and clean import. Now, you can 

```
R
library(CDL)
```

You have the library installed and you can use the basic interface. I
created a few graphical interfaces and a few pre-defined
parameters. Two methods such Holt-Winters and Moving Average model the
behavior of the time series and this little demo will not convey much
the different power of the methods. In the following, creatM is a
function creating a time series that increase in average exponentially

```

SmolatestXMEarlyP(10,5,createM,"Non-Parametric",300)
SmolatestXMEarlyP(10,5,createM,"Kernels",300)
SmolatestXMEarlyP(10,5,createM,"Compression",300) 
SmolatestXMEarlyP(10,5,createM,"Martingale",300)
SmolatestXMEarlyP(10,5,createM,"Holt-Winters",300)  

```

This should give an taste of the different approaches. They use the
default parameters written in CDLR.R files
```
 * HoltWintersMethod       0    P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }                                                                                                                        
# * NonParametricMethod     1    P = (Rsize, Wsize, 1, Ratio=0.2, MST=0/POS=1,1.0)                                                                                                            
# * MovingAverageMethod     2    P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }                                                                                                                        
# * MartingaleMethod        3    P = (e=0.93, Dt=3,MAX=20, Ratio=0.2, Wsize, 0=n^2/1=average/2=PDF)                                                                                           
# * CompressionMethod       4    P = (Rsize,Wsize,1,#BOOTSTRAP)                                                                                                                               
# * KernelMethodMMD         5    P = (Rsize,Wsize,RBF=1,MMD^2=1/Linear=0)                                                                                                                     
# *                                                                                                                                                                                           
# *                                                                                                                                                                                           

# examples of parameters array                                                                                                                                                                
PHW  = c(0, 0.3, 0.2, 0, 2, 0)     # Holt-Winters                                                                                                                                             
PNON = c(200,200,1,0.2,0,1.0)      # Non-Parametric                                                                                                                                           
PMV  = c(20, 0.3, 0.2, 0.4, 2, 0)  # Moving Average                                                                                                                                           
PMAR = c(0.92, 3, 20, 400, 0)      # Martingale                                                                                                                                               
PKS = c(200,200,1,10)              # Compression                                                                                                                                              
PKER = c(250,250,1,0)              # Kernel                                                                                                                                                   
```


You could manipulate the methods. The non parametric uses by default
the MST approach PNON[5]=0. 


