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
#ifndef DOUBLYCOMPENSATEDSUMMATION 
#define DOUBLYCOMPENSATEDSUMMATION 1
#include <interface.h>


#ifndef DOUBLY_COMPENSATED_MODULE
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

Mat  sum(Mat *x, int n);
Mat  dcsum(Mat *x, int n);
Mat  *partialdcsum(Mat *x, int n);
Mat  sorted_dcsum(Mat *x, int n); 
Mat  sorted_sum(Mat *x, int n); 
Mat  *DistributionFunctionFromProbabilityDistribution(Mat *p, int N, int EnforceDistribution);
Mat  *DistributionFunctionFromHistogram(Mat *p, int N, int EnforceDistribution);
Mat  *ProbabilityDistributionFromHistogram(Mat *h, int N);
void DFFromPDF(Mat *df, Mat *pdf,int N, int EnforceDistribution);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


#define COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,F0,Fk)  {	\
    x_k = F0;							\
    s_k =  x_k;							\
    c_k = 0;							\
    for (k = 1; k <n; k++) {					\
      x_k = Fk;							\
      y_k = c_k +  x_k;						\
      u_k = x_k - (y_k-c_k);					\
      t_k = y_k + s_k;						\
      v_k = y_k-(t_k-s_k);					\
      z_k = u_k+v_k;						\
      s_k = t_k+z_k;						\
      c_k = z_k-(s_k-t_k);					\
    }								\
}

#define COREDCSUMK(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,F0,Fk,D0,Dk)  {	\
    x_k = F0;							\
    D0 = s_k =  x_k;						\
    c_k = 0;							\
    for (k = 1; k <n; k++) {					\
      x_k = Fk;							\
      y_k = c_k +  x_k;						\
      u_k = x_k - (y_k-c_k);					\
      t_k = y_k  + s_k;						\
      v_k = y_k-(t_k-s_k);					\
      z_k = u_k+v_k;						\
      s_k = t_k+z_k;						\
      Dk = s_k;							\
      c_k = z_k-(s_k-t_k);					\
    }								\
}




#endif
