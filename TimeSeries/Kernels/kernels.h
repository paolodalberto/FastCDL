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
#ifndef KERNELSH
#define KERNELSH 1

#include <pvalue.h>
#include <nonparametric.h>
#include <math.h>
#include <sort.h>

#define KLINEAR 0
#define KMMD1   1  /* N^2 method */
#define KMMD2   2
#define KMMD3   3

#define KERGAUSSIAN   0
#define KERRBF        1  /* N^2 method */
#define KERLINEAR     2 
#define KERNPOLY      3


typedef double (*ScalarProduct)(Mat *a, Mat *b, int n);

typedef SinglePoint *(*FeatureSpaceFunction)(SinglePoint *a);

typedef double (*KernelFunctionP)(SinglePoint *a, 
				     SinglePoint *b, 
				     FeatureSpaceFunction f,
				     ScalarProduct sp,
				     double *par, int lpar);

typedef double (*KernelFunction)(SinglePoint *a, 
				    SinglePoint *b, 
				    double *par, int lpar,
				    ScalarProduct f);

// how to hide the data and method for the h(z_i,z_j)
struct h_data { 
  SinglePoint *xi;  // z_i
  SinglePoint *yi;
  SinglePoint *xj;  // z_j
  SinglePoint *yj;

  double *sigmasquare;  /* we do not really care about these in general */
  int lp;

  KernelFunction k;
  FeatureSpaceFunction ph;
  ScalarProduct  ip;
  
};

typedef struct h_data Hdata;

typedef double (*KernelFunctionH)(Hdata *hdata);




#ifndef KERNELS_METHODS_MODULE 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif



extern int kernelMethods(TimeSeries *stream,
			 NonParametricState **state,
			 TimeSeries **val,  TimeSeries **pval,
			 int N, int M, int KM, int KK);





#if defined(c_plusplus) || defined(__cplusplus)
}
#endif



#endif //KERNELS_METHODS_MODULE 


#endif //KERNELSH
