/******************************************************
  Copyright 2019 Paolo D'Alberto.

  This file is part of the Fast Change Detection Library
  (Multidimensional) FastCDL

  FastCDL is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  FastCDL is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with FastCDL.  If not, see <http://www.gnu.org/licenses/>.

*/
#ifndef DISTANCEFUNCTION
#define  DISTANCEFUNCTION 1

#define FNumber 14
#define FParNumber 4



#include <type.h>



typedef double (*DistanceFunctions)(Mat *, Mat *, int );
typedef double (*DistanceFunctionsPar)(Mat *, Mat *, int , double);



#ifndef DISTANCE_MODULE
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  
  extern double PAR[FParNumber];
  extern DistanceFunctions F[FNumber];
  extern DistanceFunctionsPar FP[FParNumber];
  extern char *FN[FNumber];
  extern char *FPN[FParNumber];

  extern double phiofA(Mat *r, Mat *w, int n);
  extern double KsiofA(Mat *r, Mat *w, int n);
  extern double KolmogorovSmirnov(  Mat *r, Mat *w, int n);
  extern double KullbackLeiberI(Mat *r, Mat *w, int n);
  extern double KullbackLeiberI(Mat *r, Mat *w, int n);
  extern double JinK(Mat *r, Mat *w, int n);
  extern double JinL(Mat *r, Mat *w, int n);
  extern double JensenShannonDivergence(Mat *r, Mat *w, int n);
  extern double ChiSquare(Mat *r, Mat *w, int n);
  extern double Hellinger(Mat *r, Mat *w, int n);
  extern double Bhattacharyya(Mat *r, Mat *w, int n);
  extern double GeneralizedKs(Mat *r, Mat *w, int n, double s);
  extern double GeneralizedKr(Mat *r, Mat *w, int n, double s);
  extern double GeneralizedK2s(Mat *r, Mat *w, int n, double s);
  extern double CramerVonMises(Mat *r, Mat *w, int n);
  extern double Euclide(Mat *r, Mat *w, int n);
  extern double Minkowsky(Mat *r, Mat *w, int n, double p);
  extern double Camberra(Mat *r, Mat *w, int n); 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

#endif
