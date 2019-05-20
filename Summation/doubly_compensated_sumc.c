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

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define DOUBLY_COMPENSATED_MODULE 1

#include <doubly_compensated_sumc.h>





#include <sort.h>
#include <quicksort.h>
//#include <quicksort.c>

#include <assert.h>

static int debug =0;



Mat sum(Mat *x, int n) { 
  Mat s,s0,s1,s2,s3;
  int i;

  if (n<4) {
    s = x[0];
    for (i=1;i<n;i++) { 
      s+=x[i];
    }
    return s;

  } else {
    s=0;
    s0 = x[0];
    s1 = x[1];
    s2 = x[2];
    s3 = x[3];
    for (i=4;i<n-8;i+=4) {
      s += s0+s1+s2+s3;
      s0 = x[i];
      s1 = x[i+1];
      s2 = x[i+2];
      s3 = x[i+3];
    }
    s += s0+s1+s2+s3;
    for (;i<n;i++) {
      s+=x[i];
    }
    
    return s;
  }
  
}

Mat sumD(Mat *x, int n) { 
  Mat s,s0,s1,s2,s3;
  int i;

  if (n<4) {
    s = x[0];
    for (i=1;i<n;i++) { 
      s+=x[i];
    }
    return s;

  } else {
    s=0;
    s0 = x[0];
    s1 = x[1];
    s2 = x[2];
    s3 = x[3];
    for (i=4;i<n-8;i+=4) {
      s += s0+s1+s2+s3;
      s0 = x[i];
      s1 = x[i+1];
      s2 = x[i+2];
      s3 = x[i+3];
    }
    s += s0+s1+s2+s3;
    for (;i<n;i++) {
      s+=x[i];
    }
    
    return s;
  }
  
}









Mat dcsum(Mat *x, int n) { 
  Mat s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k;
  int k;
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,x[0],x[k]);

  return s_k;
}

Mat sorted_dcsum(Mat *x, int n) { 
  
  SORT(x,n);
  return dcsum(x,n);

}
Mat sorted_sum(Mat *x, int n) { 
  
  SORTI(x,n);
  return sum(x,n);

}


Mat* partialdcsum(Mat *x, int n) { 
  Mat c_k,s_k,y_k,u_k,t_k,v_k,z_k,x_k;
  int k=0;

  Mat *s = (Mat*) calloc(n,sizeof(Mat));
  assert(s);
 
  COREDCSUMK(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,x[0],x[k],s[0],s[k]);

  return s;
}


Mat * DistributionFunctionFromProbabilityDistribution(Mat *p, int N, int EnforceDistribution) {
  int i;
  Mat *df = partialdcsum(p,N);
  if(debug)  {
    printPDF_DFHistogram(p,N);
    printPDF_DFHistogram(df,N);
  }
  if (EnforceDistribution) {
    for (i=0; i <N;i++) {
      if (df[i]>1 )  {
        df[i]= 1; 
      }
    }
  }
  return df;
  
}


//inline
void pdcsum(Mat *s, Mat *x, int n) { 
  Mat c_k,s_k,y_k,u_k,t_k,v_k,z_k,x_k;
  int k=0;

  COREDCSUMK(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,x[0],x[k],s[0],s[k]);

}


void  DFFromPDF(Mat *df, Mat *p, int N, int EnforceDistribution) {
  int i;
  pdcsum(df,p,N);
  if(debug)  {
    printPDF_DFHistogram(p,N);
    printPDF_DFHistogram(df,N);
  }
  if (EnforceDistribution) {
    for (i=0; i <N;i++) {
      if (df[i]>1 )  {
        df[i]= 1; 
      }
    }
  }
}

Mat *ProbabilityDistributionFromHistogram(Mat *h, int N) {
  
  Mat *pdf = (Mat *) calloc(N,sizeof(Mat));
  int i;
  Mat d = sumD(h,N);
  

  assert(d);
  
  for (i=0;i<N;i++) { 
    pdf[i] = h[i]/((Mat)d);
  }

  return pdf;
}
Mat *DistributionFunctionFromHistogram(Mat *h, int N,int EnforceDistribution) {
  Mat c_k,s_k,y_k,u_k,t_k,v_k,z_k,x_k;

  Mat *df = (Mat *) calloc(N,sizeof(Mat));
  int i,k;
  Mat d = sumD(h,N);

  assert(d);
  
  
  COREDCSUMK(N,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,((Mat)h[0]/d),((Mat)h[k]/d),df[0],df[k]);
 
  if (EnforceDistribution) {
    for (i=0; i <N;i++) {
      if (df[i]>1 )  {
        df[i]= 1; 
      }
    }
  }
  return df;
}
