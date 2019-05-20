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
#define DISTANCE_MODULE
#include <distance.h>

#include <doubly_compensated_sumc.h>
#include <math.h>
#include <stdio.h>

static int debug=0;

#define MAX(a,b) (((a)<(b))?(b):(a))
#define MIN(a,b) (((a)>(b))?(b):(a))


double phiofA(Mat *r, Mat *w, int n) { 
  
  int i;
  double sup=0; 
  double d,p;
  double b;
  if (debug) { 
      printf("distanceMeasure phiofA \n");
  }

  for (i=0;i<n;i++) {
    if (debug) { 
      printf("r[i] = %f  w[i] = %f\n",r[i],w[i]);
    }

  
    d = r[i]-w[i];
    p = (r[i]+w[i])/2;
    if (0<p  && p <1) {
      b = (fabs(d)/sqrt(MIN(p,1-p)));
      
      if (debug) { 
	printf("b = %f \n",b);
      }

      sup = MAX(sup,b);
    }
  }
  return sup;
}

double KsiofA(Mat *r, Mat *w, int n) { 
  
  int i;
  double sup=0; 
  double d,p;
  double b;
  
  for (i=0;i<n;i++) {
    d = r[i]-w[i];
    p = (r[i]+w[i])/2;
    if (0<p  && p <1) {
      b = (fabs(d)/sqrt(p*(1-p)));
      sup = MAX(sup,b);
      if (debug) { 
	printf("sup = %f \n",sup);
      }
    }
  }

  return sup;
}

double KolmogorovSmirnov(  Mat *r, Mat *w, int n) {
  
  int i;
  double sup=0; 
  double d;
  
  for (i=0;i<n;i++) {
    d = fabs(r[i]-w[i]);
    sup = MAX(sup,d);
  }

  return sup;


}
double VariationDistance(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    fabs(r[0]-w[0]),
	    fabs(r[k]-w[k]));
  
  return s_k;
    
}

//inline
double zlog_x_y(double z, double x, double y) {

  if (z==0 || x==0 || y==0) 
    return 0;
  else 
    return z*(double)log2((double)x/y);
	      
}

double KullbackLeiberI(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    zlog_x_y(r[0],r[0],w[0]),
	    zlog_x_y(r[k],r[k],w[k]))


  return s_k;
}

double KullbackLeiberJ(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    zlog_x_y(r[0]-w[0],r[0],w[0]),
	    zlog_x_y(r[k]-w[k],r[k],w[k]))


  return s_k;
}

double JinK(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    zlog_x_y(r[0],2*r[0],r[0]+w[0]),
	    zlog_x_y(r[k],2*r[k],r[k]+w[k]))

  return s_k;
}

double JinL(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    (zlog_x_y(r[0],2*r[0],r[0]+w[0])+zlog_x_y(w[0],2*w[0],r[0]+w[0])),
	    (zlog_x_y(r[k],2*r[k],r[k]+w[k])+zlog_x_y(w[k],2*w[k],r[k]+w[k])))

  return s_k;
}

double JensenShannonDivergence(Mat *r, Mat *w, int n) { 

  return 0.5*JinL(r,w,n);


}

double ChiSquare(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0])?(r[0]-w[0])*(r[0]-w[0])/r[0]:0),
	    ((r[k])?(r[k]-w[k])*(r[k]-w[k])/r[k]:0))

  return s_k;


}

//inline
double H(double x, double y) { 

  double t = sqrt(x) - sqrt(y);
  t *=t;
  
  return t;
}

double Hellinger(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    H(r[0],w[0]),
	    H(r[k],w[k]));
  
  return s_k;
}

double Bhattacharyya(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    sqrt(r[0]*w[0]),
	    sqrt(r[k]*w[k]));
  
  return s_k;
}

//inline
double Kr(Mat *r, Mat *w, int n, double rr) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0])?pow(r[0],rr):0)*((w[0])?pow(w[0],1-rr):0),
	    ((r[k])?pow(r[k],rr):0)*((w[k])?pow(w[k],1-rr):0)
	    );

  
  return log2((double)s_k)/(rr-1.0);
}
//inline
double Ks(Mat *r, Mat *w, int n, double s) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0])?pow(r[0],s):0)*((w[0])?pow(w[0],1-s):0),
	    ((r[k])?pow(r[k],s):0)*((w[k])?pow(w[k],1-s):0)
	    );

  
  return (s_k-1)/(s-1);
}


//inline
double K2s(Mat *r, Mat *w, int n, double s) { 
    
  return Ks(r,w,n,s)/s;
}


double GeneralizedKs(Mat *r, Mat *w, int n, double s) { 
    if (debug) { 
    printf("distanceMeasure GeneralizedKs n=%d s=%f\n",n,s);
  }

  if (s==1) return KullbackLeiberI(r,w,n);
  else return Ks(r,w,n,s);
}

double GeneralizedKr(Mat *r, Mat *w, int n, double s) { 
  
  if (s==1) return KullbackLeiberI(r,w,n);
  else return Kr(r,w,n,s);
}

double GeneralizedK2s(Mat *r, Mat *w, int n, double s) { 
  
  if (s==0) return KullbackLeiberI(w,r,n);
  if (s==1) return KullbackLeiberI(r,w,n);
  else return K2s(r,w,n,s);
}




//inline
double CV(double x, double y) { 

  double t = x - y;
  t *=t;
  
  return t;
}

double CramerVonMises(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    CV(r[0],w[0]),
	    CV(r[k],w[k]));
  
  return s_k;
}

double Euclide(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    CV(r[0],w[0]),
	    CV(r[k],w[k]));
  
  return sqrt(s_k);
}

double Minkowsky(Mat *r, Mat *w, int n, double p) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0]-w[0])?pow(fabs(r[0]-w[0]),p):0),
	    ((r[0]-w[0])?pow(fabs(r[k]-w[k]),p):0));
  
  //  printf("Mink %f \n",s_k);
  return ((s_k)?pow(s_k,1/p):0);
}

double Camberra(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0]+w[0])?(fabs(r[0]-w[0])/(r[0]+w[0])):0),
	    ((r[k]+w[k])?(fabs(r[k]-w[k])/(r[k]+w[k])):0));

  
  return (s_k);
}

double PAR[FParNumber] = { 2, 2, 2, 3};

DistanceFunctions F[FNumber] = {
  phiofA,
  KsiofA,
  KolmogorovSmirnov,
  KullbackLeiberI,
  KullbackLeiberJ,
  JinK,
  JinL,
  JensenShannonDivergence,
  ChiSquare,
  Hellinger,
  Bhattacharyya,
  CramerVonMises,
  Euclide,
  Camberra
};
DistanceFunctionsPar FP[FParNumber] = {
  GeneralizedKs,
  GeneralizedKr,
  GeneralizedK2s,
  Minkowsky
};
char *FN[FNumber] = {
  "phiofA",
  "KsiofA",
  "KolmogorovSmirnov",
  "KullbackLeiberI",
  "KullbackLeiberJ",
  "JinK",
  "JinL",
  "JensenShannonDivergence",
  "ChiSquare",
  "Hellinger",
  "Bhattacharyya",
  "CramerVonMises",
  "Euclide",
  "Camberra"
};
char *FPN[FParNumber] = {
  "GeneralizedKs",
  "GeneralizedKr",
  "GeneralizedK2s",
  "Minkowsky"
};



