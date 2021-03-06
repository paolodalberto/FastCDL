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
#ifndef MARTIGALE_METHODS
#define MARTIGALE_METHODS 


#include<string.h>
#include<type.h>
#include<math.h>
#include<window.h>
#include<stdlib.h>
#include<stdio.h>
#include<pdf.h>

// interface for the alpah strangenes measure
//#define RDEF 

#ifdef RDEF 
#include <R.h>
#define PRINTF Rprintf
#else 
#define PRINTF printf
#endif

#define N_MARTINGALES 2
#define INITIALIZE_M  {1.0,1.0} 
#define INITIALIZE_TRANSDUCER  {power_martingale, power_martingale} 

typedef double (*Transducer)(double pval, double m, double par);  




struct sstate { 
  
  int max;
  double epsilon;  double t; double  lambda;  
  double fraction;
  double     *prev_martingale;
  Transducer *fs;
 
  
  // previous samples
  Window *w;
  // series of p-values
  Window *pval;    // 1 dimensional 
  // uniform reference 
  Window *unif_reference; // 1 dimensional 


  Adj *a;
  

} ;



typedef struct sstate MartingaleState;


typedef double (*Strangeness_pvalue_statistical_window)(StatisticalWindow *sw, SinglePoint *sp);
typedef double (*Strangeness_pvalue)(TimeSeries *ts, SinglePoint *sp, void *state);
typedef double *(*Alpha_i)(TimeSeries *ts);
typedef TimeSeries* (*Martingale)(double *prev, double epsilon, double t, double lambda, TimeSeries *ts, void *state);


// coefficients as specified on Shen-Shyang Ho " A martingale framework for concept change in detection in time-varying data streams" ICML 2005
#define StochasticMartingale(ts,state,val,pval,ym,m) stochastic_martingale(ts,state,(val),(pval),(ym),0.92,3,20,m,stochastic_distance_strangeness_pvalue)

#define ALLOCATE_MARTINGALE_STATE(s)  {                           \
  s = (MartingaleState*) calloc(1,sizeof(MartingaleState));         \
  s->w = s->pval=s->unif_reference=0;                             \
  s->epsilon=s->t =s->lambda =0; s->max = 0;\
  s->prev_martingale = (double*) calloc(N_MARTINGALES,sizeof(double));  memcpy( s->prev_martingale,_prev_martingale,2*sizeof(double));s->fs = _fs; \
} 

#define FREE_MARTINGALE_STATE(s) { \
  if (s) {        \
     if (s->prev_martingale) {free(s->prev_martingale); s->prev_martingale = 0;}\
     FREEW(s->w);    \
     FREEW(s->pval); \
     FREEW(s->unif_reference);\
     if ((s)->a)  {FREEADJ((s)->a);  }\
     free(s); s=0;\
  } \
      }

#define PRINTMARTINGALESTATE(s) { free(PrintMartingaleState(s,0,1)); }




#ifndef MARTINGALE_MODULE
#if defined(c_plusplus) || defined(__cplusplus) 
extern "C" {
#endif
  extern int binary_search_sorted_series(TimeSeries *ts, SinglePoint *sp);
  extern double stochastic_distance_strangeness_pvalue_statistical_window(Window *w, SinglePoint *sp);
  extern double stochastic_distance_strangeness_pvalue(TimeSeries *ts, SinglePoint *sp, void *state);

  extern int  conformal_prediction_martingale(TimeSeries *ts,
					      MartingaleState **state, TimeSeries **val, TimeSeries **pval, TimeSeries **res,  
					      double epsilon,  double t,  double lambda, 
					      double fraction, int M,  Strangeness_pvalue pvalueofDistanceFunction); 
  extern MartingaleState *ReadMARPState(int identation, char **temp);
  extern char *PrintMartingaleState(MartingaleState *t, int identation, int P);
  extern double power_martingale(double pval, double m, double par); 
#if defined(c_plusplus) || defined(__cplusplus) 
}
#endif 

#endif //#ifndef MARTINGALE_MODULE


#endif // #ifndef MARTIGALE_METHODS 
