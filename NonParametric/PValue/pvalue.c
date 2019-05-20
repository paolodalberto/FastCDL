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
#define PVALUE_MODULE
#include <pvalue.h>


int  debug=0;
Mat fN(register unsigned int x)       { return (Mat) x;              } 
Mat f1_N(register unsigned int x)     { return (Mat) (1/(Mat) x);            } 
Mat f_sN(register unsigned int x)     { return sqrt((double) x);        } 
Mat f_1_sN(register unsigned int x)   { return 1/sqrt((double) x);      } 
Mat f_sN_log(register unsigned int x) { return (Mat) (sqrt((double) x)/(log2((double) x)));} 
Mat f_i(register unsigned int x)      { return 1;              }
Mat f_square(register unsigned int x) { return (Mat) x*x;            }
Mat f_lN(register unsigned int x)     { return (Mat) log2((double) x);        }




Nomalization N[(CAMBERRA+1)] = { 
  f_sN_log,     //#define PHIOFA 0		       
  f_sN_log, 	//#define KSIOFA 1		       
  f_sN, 	//#define KOLMOGOROVSMIRNOV 2 	       
  f_sN,  	//#define KOLMOGOROVSMIRNOVR 3	       
  f_1_sN,	//#define KULLBACKLEIBERI    4 	       
  f_i,		//#define KULLBACKLEIBERJ    5	       
  f_1_sN,	//#define JINK               6	       
  f_i,		//#define JINL               7	       
  f_i,		//#define JENSENSHANNONDIVERGENCE  8     
  f_square,	//#define SIMPLIFIEDWILCOX         9     
  f_i,		//#define SIMPLIFIEDTTEST          10    
  f_i,		//#define SIMPLIFIEDCHISQUARE     11     
  f_i,		//#define CHISQUARE               12     
  f_1_sN,	//#define VARIATIONALDISTANCE     13     
  f_i,		//#define HELLINGER        14	       
  f_1_sN,	//#define BHATTACHARYYA 15	       
  f_i,		//#define GENERALIZEDKR 16	       
  f_i,		//#define GENERALIZEDKS 17	       
  f_i,		//#define GENERALIZEDK2S 18	       
  f_i,		//#define CRAMERVONMISES 19	       
  f_i,		//#define EUCLID         20	       
  f_lN,		//#define MINKOWSKY      21	       
  f_1_sN	//#define CAMBERRA       22                 
};

// Number of pvalues 
Mat PVAL[COL] = PBRAKETS; // { 0.95, 0.96, 0.97, 0.98, 0.99, 0.999, 0.9999 }

// Table generate by simulation using R

Mat TABLE_PVALUE[TOTAL_DISTANCE_FUNCTIONS*COL] = {
  0.4638322 , 0.4759702 , 0.4913385 , 0.5133822 , 0.5464084 , 0.6531130 , 0.7510163   ,   //#define PHIOFA 0		       	    
  0.4949757 , 0.5084157 , 0.5267518 , 0.5502331 , 0.5921779 , 0.7212549 , 0.8440449   ,   //#define KSIOFA 1		       	    
  1.900658  , 1.962991  , 2.024656  , 2.124265  , 2.276840  , 2.735264  , 3.150000    ,   //#define KOLMOGOROVSMIRNOV 2 	       	    
  1.900658  , 1.962991  , 2.024656  , 2.124265  , 2.276840  , 2.735264  , 3.150000    ,   //#define KOLMOGOROVSMIRNOVR 3	       	    
  2.041653  , 2.172348  , 2.335706  , 2.556067  , 2.910720  , 3.912149  , 4.845052    ,   //#define KULLBACKLEIBERI    4 	       	    
  7.524244  , 8.117583  , 8.882838  , 9.960862  , 11.830194 , 18.498500 , 24.712980   ,   //#define KULLBACKLEIBERJ    5	       	    
  0.9921603 , 1.0560659 , 1.1336015 , 1.2386034 , 1.4031260 , 1.8768068 , 2.2778924   ,   //#define JINK               6	       	    
  1.884070  , 2.031107  , 2.221361  , 2.488194  , 2.956818  , 4.610568  , 6.159997    ,   //#define JINL               7	       	    
  0.9420352 , 1.0155534 , 1.1106803 , 1.2440969 , 1.4784090 , 2.3052838 , 3.0799985   ,   //#define JENSENSHANNONDIVERGENCE  8        
  0.5263592 , 0.5290494 , 0.5328250 , 0.5385530 , 0.5493000 , 0.5864000 , 0.6213000   ,   //#define SIMPLIFIEDWILCOX         9        
  1.647216  , 1.755087  , 1.881376  , 2.058652  , 2.329090  , 3.072531  , 3.698181    ,   //#define SIMPLIFIEDTTEST          10       
  0.000000  , 0.00000   , 0.000000  , 0.000000  , 0.000000  , 0.000000  , 0.000000    ,   //#define SIMPLIFIEDCHISQUARE     11        
  5.322960  , 5.733091  , 6.267591  , 7.105757  , 8.520936  , 13.694750 , 20.300115   ,   //#define CHISQUARE               12        
  1.644968  , 1.715000  , 1.803829  , 1.923313  , 2.120613  , 2.678109  , 3.172124    ,   //#define VARIATIONALDISTANCE     13        
  0.6559146 , 0.7074165 , 0.7723961 , 0.8659146 , 1.0285145 , 1.6042294 , 2.1852986   ,   //#define HELLINGER        14	       	    
  3610906   , 4000275   , 4000546   , 4000687   , 4000791   , 4000899   , 4000934     ,   //#define BHATTACHARYYA 15	       	    
  -6031.468 , -2669.301 , -2662.673 , -2656.554 , -2648.997 , -2632.346 , -2619.024   ,   //#define GENERALIZEDKR 16	       	    
  0.5783383 , 0.5872143 , 0.6004118 , 0.6211828 , 0.6647824 , 0.8464969 , 1.1748277   ,   //#define GENERALIZEDKS 17	       	    
 -199819.77 , -53920.15 , -53305.09 , -52739.89 , -52053.85 , -50637.40 ,  -49578.49  ,   //#define GENERALIZEDK2S 18	       	    
  1.844373  , 1.991170  , 2.188533  , 2.479725  , 2.958698  , 4.568506  , 6.345529    ,   //#define CRAMERVONMISES 19	       	    
  1.358077  , 1.411088  , 1.479369  , 1.574714  , 1.720087  , 2.137406  , 2.519033    ,   //#define EUCLID         20	       	    
  4.185407  , 4.342682  , 4.542520  , 4.819563  , 5.244374  , 6.501403  , 7.552654    ,   //#define MINKOWSKY      21	       	    
  2.955362  , 3.072861  , 3.218624  , 3.414089  , 3.733291  , 4.677509  , 5.492240   };	  //#define CAMBERRA       22                 

// routine to search the above table
//inline
Mat search_pvalue(Mat x, unsigned int N, Mat *ptable, Mat *pval,int dnumber, Nomalization f) { 


  int i;
  Mat nres = x*f(N); 

  if(debug) { 
    printf("SEARCH PTABLE func %d  value %f N value %f \n",dnumber,x,nres);
    for (i=0;i<COL;i++) { 
      printf("%f ",ptable[dnumber*COL+i]);
    }
    printf("\n");
  }

  if (nres < ptable[dnumber*COL])  { 
    return 0;
  }
  for (i=1;i<COL;i++) { 
    if (nres <= ptable[dnumber*COL+i])  { 
      return pval[i];
    }
    
  }

  return 1;
}




#ifndef MIN
#define MAX(a,b) (((a)<(b))?(b):(a))
#define MIN(a,b) (((a)>(b))?(b):(a))
#endif

/***********************************************
 * Distance functions
 *
 *
 */

Statistic phiofAWithPValue(Mat *r, Mat *w, int n) { 
  
  int i;
  double sup=0; 
  double d,p;
  double b;
  Statistic result;


  if (debug) { 
      printf("distanceMeasure pvalue phiofA \n");
  }

  for (i=0;i<n;i++) {

  
    d = r[i]-w[i];
    p = (r[i]+w[i])/2;
    if (debug) { 
      printf("r[i] = %f  w[i] = %f d = %f p=%f \n",r[i],w[i],d,p);
    }
    if (0<p  && p <1) {
      b = (fabs(d)/sqrt(MIN(p,1-p)));
      
      if (debug) { 
	printf("b = %f sup = %f \n",b,sup);
      }

      sup = MAX(sup,b);
    }
  }


  result.val = sup;
  result.pval = search_pvalue(sup, n,TABLE_PVALUE, PVAL,PHIOFA, N[PHIOFA]);
  
  return result;
}




Statistic KsiofAWithPValue(Mat *r, Mat *w, int n) { 
  
  int i;
  double sup=0; 
  double d,p;
  double b;
  Statistic result;

  if (debug) { 
      printf("distanceMeasure pvalue ksiofA \n");
  }



  for (i=0;i<n;i++) {
    
    d = r[i]-w[i];
    p = (r[i]+w[i])/2;
    if (debug) { 
	printf("r[i] = %f  w[i] = %f d = %f p=%f \n",r[i],w[i],d,p);
    }
    if (0<p  && p <1) {
      b = (fabs(d)/sqrt(p*(1-p)));
      sup = MAX(sup,b);
      if (debug) { 
	printf("sup = %f \n",sup);
      }
    }
  }

  result.val = sup;
  result.pval = search_pvalue(sup, n,TABLE_PVALUE, PVAL,KSIOFA, N[KSIOFA]);
   
  return result;
}

Statistic KolmogorovSmirnovWithPValue(  Mat *r, Mat *w, int n) {
  
  int i;
  double sup=0; 
  double d;
  Statistic result;
  
  for (i=0;i<n;i++) {
    d = fabs(r[i]-w[i]);
    sup = MAX(sup,d);
  }

  result.val = sup;
  result.pval = search_pvalue(sup, n,TABLE_PVALUE, PVAL,KOLMOGOROVSMIRNOV, N[KOLMOGOROVSMIRNOV]);
  return result;


}
Statistic VariationDistanceWithPValue(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    fabs(r[0]-w[0]),
	    fabs(r[k]-w[k]));
  


  result.val = s_k;
  result.pval = search_pvalue(result.val, n,TABLE_PVALUE, PVAL,VARIATIONALDISTANCE, N[VARIATIONALDISTANCE]);
  return result;
    
}

static inline double zlog_x_y(double z, double x, double y) {

  if (z==0 || x==0 || y==0) 
    return 0;
  else 
    return z*log2(x/y);
	      
}

Statistic KullbackLeiberIWithPValue(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    zlog_x_y(r[0],r[0],w[0]),
	    zlog_x_y(r[k],r[k],w[k]))


  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,KULLBACKLEIBERI, N[KULLBACKLEIBERI]);
  return result;

}

Statistic KullbackLeiberJWithPValue(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    zlog_x_y(r[0]-w[0],r[0],w[0]),
	    zlog_x_y(r[k]-w[k],r[k],w[k]))


  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,KULLBACKLEIBERJ, N[KULLBACKLEIBERJ]);
  return result;

}

Statistic JinKWithPValue(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
   Statistic result;
 
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    zlog_x_y(r[0],2*r[0],r[0]+w[0]),
	    zlog_x_y(r[k],2*r[k],r[k]+w[k]))

  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,JINK, N[JINK]);
  return result;
}

Statistic JinLWithPValue(Mat *r, Mat *w, int n) {
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
   Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    (zlog_x_y(r[0],2*r[0],r[0]+w[0])+zlog_x_y(w[0],2*w[0],r[0]+w[0])),
	    (zlog_x_y(r[k],2*r[k],r[k]+w[k])+zlog_x_y(w[k],2*w[k],r[k]+w[k])))

  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,JINL, N[JINL]);
  return result;
}

Statistic JensenShannonDivergenceWithPValue(Mat *r, Mat *w, int n) { 

  Statistic result;
  Mat s_k = 0.5*JinL(r,w,n);

  result.val = s_k;
  result.pval = search_pvalue(result.val, n,TABLE_PVALUE, PVAL,JENSENSHANNONDIVERGENCE, N[JENSENSHANNONDIVERGENCE]);
  return result;


}

Statistic ChiSquareWithPValue(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;

  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0])?(r[0]-w[0])*(r[0]-w[0])/r[0]:0),
	    ((r[k])?(r[k]-w[k])*(r[k]-w[k])/r[k]:0))

  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,CHISQUARE, N[CHISQUARE]);
  return result;



}

static inline double H(double x, double y) { 

  double t = sqrt(x) - sqrt(y);
  t *=t;
  
  return t;
}

Statistic HellingerWithPValue(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    H(r[0],w[0]),
	    H(r[k],w[k]));
  
  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,HELLINGER, N[HELLINGER]);
  return result;
}

Statistic BhattacharyyaWithPValue(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    sqrt(r[0]*w[0]),
	    sqrt(r[k]*w[k]));
  
  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,BHATTACHARYYA, N[BHATTACHARYYA]);
  return result;
}

//inline
Statistic KrWithPValue(Mat *r, Mat *w, int n, double rr) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0])?pow(r[0],rr):0)*((w[0])?pow(w[0],1-rr):0),
	    ((r[k])?pow(r[k],rr):0)*((w[k])?pow(w[k],1-rr):0)
	    );

  result.val = log2(s_k)/(rr-1);
  result.pval = 0;
  return result;
}
//inline
Statistic KsWithPValue(Mat *r, Mat *w, int n, double s) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0])?pow(r[0],s):0)*((w[0])?pow(w[0],1-s):0),
	    ((r[k])?pow(r[k],s):0)*((w[k])?pow(w[k],1-s):0)
	    );

  
  result.val = (s_k-1)/(s-1);
  result.pval = 0;
  return result;
}


//inline
Statistic K2sWithPValue(Mat *r, Mat *w, int n, double s) { 
  Statistic result =   KsWithPValue(r,w,n,s);
  result.val /=s;
  return result;
}


Statistic GeneralizedKsWithPValue(Mat *r, Mat *w, int n, double s) { 
    if (debug) { 
    printf("distanceMeasure pvalue GeneralizedKs n=%d s=%f\n",n,s);
  }

  if (s==1) return KullbackLeiberIWithPValue(r,w,n);
  else return KsWithPValue(r,w,n,s);
}

Statistic GeneralizedKrWithPValue(Mat *r, Mat *w, int n, double s) { 
  
  if (s==1) return KullbackLeiberIWithPValue(r,w,n);
  else return KrWithPValue(r,w,n,s);
}

Statistic GeneralizedK2sWithPValue(Mat *r, Mat *w, int n, double s) { 
  
  if (s==0) return KullbackLeiberIWithPValue(w,r,n);
  if (s==1) return KullbackLeiberIWithPValue(r,w,n);
  else return K2sWithPValue(r,w,n,s);
}




static inline double CV(double x, double y) { 

  double t = x - y;
  t *=t;
  
  return t;
}

Statistic CramerVonMisesWithPValue(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    CV(r[0],w[0]),
	    CV(r[k],w[k]));
  
  result.val = s_k;
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,CRAMERVONMISES, N[CRAMERVONMISES]);
  return result;
}

Statistic EuclideWithPValue(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    CV(r[0],w[0]),
	    CV(r[k],w[k]));
  
  result.val = sqrt(s_k);
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,EUCLID, N[EUCLID]);

  return result;

}

Statistic MinkowskyWithPValue(Mat *r, Mat *w, int n, double p) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  
  Statistic result;

  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0]-w[0])?pow(fabs(r[0]-w[0]),p):0),
	    ((r[0]-w[0])?pow(fabs(r[k]-w[k]),p):0));
  
  result.val =  ((s_k)?pow(s_k,1/p):0);
  result.pval = search_pvalue(result.val,n, TABLE_PVALUE, PVAL,MINKOWSKY, N[MINKOWSKY]);

  return result;
}

Statistic CamberraWithPValue(Mat *r, Mat *w, int n) { 
  Mat x_k,s_k,c_k,y_k,u_k,t_k,v_k,z_k;
  int k;
  Statistic result;
  
  COREDCSUM(n,k,s_k,c_k,y_k,u_k,t_k,v_k,z_k,x_k,
	    ((r[0]+w[0])?(fabs(r[0]-w[0])/(r[0]+w[0])):0),
	    ((r[k]+w[k])?(fabs(r[k]-w[k])/(r[k]+w[k])):0));

  
  result.val =  (s_k);
  result.pval = search_pvalue(result.val, n,TABLE_PVALUE, PVAL,CAMBERRA, N[CAMBERRA]);
  
  return result;
}


DistanceFunctionsPvalue FPV[FNumber] = {
  phiofAWithPValue,
  KsiofAWithPValue,
  KolmogorovSmirnovWithPValue,
  KullbackLeiberIWithPValue,
  KullbackLeiberJWithPValue,
  JinKWithPValue,
  JinLWithPValue,
  JensenShannonDivergenceWithPValue,
  ChiSquareWithPValue,
  HellingerWithPValue,
  BhattacharyyaWithPValue,
  CramerVonMisesWithPValue,
  EuclideWithPValue,
  CamberraWithPValue
};

DistanceFunctionsPvaluePar FPPV[FParNumber] = {
  GeneralizedKsWithPValue,
  GeneralizedKrWithPValue,
  GeneralizedK2sWithPValue,
  MinkowskyWithPValue
};






// Specialized sub sets 
double *SANTANU_PAR= 0;

DistanceFunctionsPvalue SANTANU_F[SANTANU_FNumber] = {
  phiofAWithPValue,
  KsiofAWithPValue,
  KolmogorovSmirnovWithPValue,
  JensenShannonDivergenceWithPValue,
  HellingerWithPValue
};

char *SANTANU_FN[SANTANU_FNumber] = {
  "phiofA",
  "KsiofA",
  "KolmogorovSmirnov",
  "JensenShannonDivergence",
  "Hellinger"
 };


DistanceFunctionsPvaluePar SANTANU_FP[SANTANU_FParNumber];
char *SANTANU_FPN[SANTANU_FParNumber];



// Anomaly detection subset

double *ANOMALY_PAR= 0;

DistanceFunctionsPvalue ANOMALY_F[ANOMALY_FNumber] = {
  phiofAWithPValue,
  KsiofAWithPValue,
  KolmogorovSmirnovWithPValue,
  KullbackLeiberJWithPValue,     
  JensenShannonDivergenceWithPValue,
  ChiSquareWithPValue,
  HellingerWithPValue,
  CramerVonMisesWithPValue,
  EuclideWithPValue,
  CamberraWithPValue
};

char *ANOMALY_FN[ANOMALY_FNumber] = {
  "phiofA",
  "KsiofA",
  "KolmogorovSmirnov",
  "KullbackLeiberJ",
  "JensenShannonDivergence",
  "ChiSquare",
  "Hellinger",
  "CramerVonMises",
  "Euclide",
  "Camberra"
 };


DistanceFunctionsPvaluePar ANOMALY_FP[ANOMALY_FParNumber];
char *ANOMALY_FPN[ANOMALY_FParNumber];








double* distanceMeasurePValue(Mat *r, Mat *w, int n,
			      Mat *result,
			      DistanceFunctionsPvalue *D, int k,
			      DistanceFunctionsPvaluePar *Dk, double *par, int kp ) { 


#if(PVALUE_MODE_0)
  char **fnames =FN;
  char **fpnames =FPN;
#elif(SANTANU_HISTOGRAM)
  char **fnames =SANTANU_FN;
  char **fpnames =SANTANU_FPN;
#elif(REGULAR_MODE)
  char **fnames =ANOMALY_FN;
  char **fpnames =ANOMALY_FPN;
#else
  char **fnames =FN;
  char **fpnames =FPN;
#endif
 
 

  int i,j;
  int N = N_METHODS; //FNumber+FParNumber;
  Statistic rs;

  if (!result) { // allocate result
    result = (Mat *) malloc(2*N*sizeof(Mat));
    assert(result);
  }

  if (debug) { 
    printf("distanceMeasure n =%d k=%d kp=%d\n",n,k,kp );
    printf("D %ld\n",(long int) D);
  }

  for (i=0;i<k;i++) {

    if(debug) { 
      printf("i = %d\n",i);
      printf("%s %ld\n",fnames[i], (long int) D[i]);
    }
    rs =  D[i](r,w,n);
    result[i] = rs.val;
    result[i+N] = rs.pval;
    if(debug) { 
      printf("%d %f %f \n",i,result[i], result[i+N]);
    }
  }
  for (j=0;j<kp;j++,i++) { 
    if(debug) { 
      printf("j = %d\n",j);
      printf("%s p=%f\n",fpnames[j],par[j]);
    }
    rs =  Dk[j](r,w,n,par[j]);
    result[i] = rs.val;
    result[i+N] = rs.pval;
    if(debug) { 
      printf("%d %f %f \n",i,result[i], result[i+N]);
    }
  }
  
  return result;

}  

static inline double *filtering(double *res, double *output) { 


  return res;
}


double* distanceHistogramsPValue(Histogram *sh1,
				 Histogram *sh2) { 

  int N;
  Mat *res,*result;
  Mat *df1,*df2;

  if (debug) { 
    printf("distanceHistogramsPValueMeasure n =%d k=%d \n",sh1->length, sh2->length );
  }

  Histogram *eh1, *eh2;
  res = (Mat *)calloc(DIM*(FNumber+FParNumber),sizeof(double));

  eh1 = (Histogram *) malloc(sizeof(Histogram)); 
  eh2 = (Histogram *) malloc(sizeof(Histogram)); 

  sorthistogram(sh1,0,sh1->length-1);
  sorthistogram(sh2,0,sh2->length-1);
  extendHistogram(sh1,sh2,eh1,eh2);
  if (debug) {
    printHistogram(sh1);
    printHistogram(sh1);
  }
  N = eh1->length;




  df1 =  DistributionFunctionFromHistogram(eh1->number,N,1);
  df2 =  DistributionFunctionFromHistogram(eh2->number,N,1);
  assert(df1 && df2); 
  if (debug) {
    printPDF_DFHistogram(df1,N);
    printPDF_DFHistogram(df2,N);
  }

  res = distanceMeasurePValue(df1,df2,N,
			res,
			FPV,FNumber,
			FPPV,PAR,FParNumber);
  
  // printResultDistance(res,FN,FNumber,FPN,FParNumber);
  result = filtering(res,result);

  //  free(res);
  free(df1);
  free(df2);
  free(eh1);
  free(eh2);
  return result;
}  



