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

```r
R
library(CDL)
```

You have the library installed and you can use the basic interface. I
created a few graphical interfaces and a few pre-defined
parameters. Two methods such Holt-Winters and Moving Average model the
behavior of the time series and this little demo will not convey much
the different power of the methods. In the following, creatM is a
function creating a time series that increase in average exponentially

```r

SmolatestXMEarlyP(10,5,createM,"Non-Parametric",300)
SmolatestXMEarlyP(10,5,createM,"Kernels",300)
SmolatestXMEarlyP(10,5,createM,"Compression",300) 
SmolatestXMEarlyP(10,5,createM,"Martingale",300)
SmolatestXMEarlyP(10,5,createM,"Holt-Winters",300)  

```

This should give an taste of the different approaches. They use the
default parameters written in CDLR.R files
```r
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

I want to share the library because anyone could use methods for the
comparisons of two D-dimensional samples. For example,

```r
source("CDLR.R")
exampleSimple(150,400,500,"temp.png")
```

The example, creates a sample of 400 vectors with dimension 150. This
is the Reference *R*. It creates a sample of 500 vectors with
dimension 150, this is what we want to compare *D* and it creates a
time series *R+R+D* and it going to compute two measures *R* vs *R*
and *R* vs *D*. 

IF you inspect the example and how the code produces the result
(time,response, and p-value) you can appreciate the plots better and
how you can use the interface to compare M and N samples (of same
dimensions).

![alt text](https://github.com/paolodalberto/FastCDL/blob/master/R/CDL/R/temp.png)
