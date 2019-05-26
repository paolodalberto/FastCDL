# FastCDL

Multidimensional Fast Change Detection and Distribution Function
comparison with and without sensitivity p-value.


This is a C library with two interface examples: An R module and
Python module. The choice of a C library is because of performance and
portability. It is easier to create an thin interface to import the C
library than to rewrite the library.

# Time Series

There is one basic interface 

    int scalarF(TimeSeries *stream,int Methods, void **state,GeneralizedOutput **out, Mat *parameters)

and there is a basic interface using only standard and low level
types. With the right combination of an integer value in the Methods
and the right values in the parameters array any user can deploy six
main methods:


* HoltWintersMethod       0
  * P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }
* NonParametricMethod     1
  * P = (Rsize, Wsize, 1, Ratio=0.2, MST=0/POS=1,1.0)
* MovingAverageMethod     2
  * P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }
* MartingaleMethod        3
  * P = (e=0.93, Dt=3,MAX=20, Ratio=0.2, Wsize, 0=n^2/1=average/2=PDF)
* CompressionMethod       4
  * P = (Rsize,Wsize,1,#BOOTSTRAP)
* KernelMethodMMD         5
  * P = (Rsize,Wsize,RBF=1,MMD^2=1/Linear=0)

As you can see in the description of the parameters, for the non
parametric method you can choose two different algorithms, for
Martingale you can use three methods, and for the kernel methods you
can choose two methods.

There is more information in the PDF paper and in the code. 

If you like to compare two M and N samples in a K-dimensional space,
just build a time series of size M+N and the time series tools is
going to go for it. In such a case you should use the non-parametric,
kernels, or compression methods (Martigale, HW and moving average are
pure time series).


# Distributions

there are 

    double* distanceHistograms(Histogram *sh1,
                               Histogram *sh2)
    double* distanceHistogramsQ(Histogram *sh1,
                               Histogram *sh2)


These two methods compare distribution, with p-values and with quorum.
It depends on the settings and how you built the library, you may use
a few CDF distance measure.
```c
  double phiofA(Mat *r, Mat *w, int n);
  double KsiofA(Mat *r, Mat *w, int n);
  double KolmogorovSmirnov(  Mat *r, Mat *w, int n);
  double KullbackLeiberI(Mat *r, Mat *w, int n);
  double KullbackLeiberI(Mat *r, Mat *w, int n);
  double JinK(Mat *r, Mat *w, int n);
  double JinL(Mat *r, Mat *w, int n);
  double JensenShannonDivergence(Mat *r, Mat *w, int n);
  double ChiSquare(Mat *r, Mat *w, int n);
  double Hellinger(Mat *r, Mat *w, int n);
  double Bhattacharyya(Mat *r, Mat *w, int n);
  double GeneralizedKs(Mat *r, Mat *w, int n, double s);
  double GeneralizedKr(Mat *r, Mat *w, int n, double s);
  double GeneralizedK2s(Mat *r, Mat *w, int n, double s);
  double CramerVonMises(Mat *r, Mat *w, int n);
  double Euclide(Mat *r, Mat *w, int n);
  double Minkowsky(Mat *r, Mat *w, int n, double p);
  double Camberra(Mat *r, Mat *w, int n); 
```