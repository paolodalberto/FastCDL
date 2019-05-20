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
#ifndef TIMESERIES 
#define TIMESERIES 

#include <stdlib.h>
#include <assert.h>
#include <type.h>
#include <window.h>
#include <holtwinters.h>
#include <nonparametric.h>
#include <movingaverage.h>
#include <martingale.h>
#include <kernels.h>
#include <pdf.h>
#include <nnstrangeness.h>
#include <compress.h>

#define HoltWintersMethod       0
#define NonParametricMethod     1
#define MovingAverageMethod     2
#define MartingaleMethod        3
#define CompressionMethod       4
#define KernelMethodMMD         5

#define PPLACE       0
#define ALPHAPLACE   1
#define BETAPLACE    2
#define GAMMAPLACE   3
#define KPLACE       4
#define SIDESPLACE   5

#define PLUSSIDE     1
#define MINUSSIDE    -1
#define BOTHSIDE     0


#define NPLACE       0
#define MPLACE       1
#define DFPLACE      2
#define QUORUMPLACE  3
#define TOPPLACE     4


#define KFUNCPLACE         2
#define KERNELMETHODPLACE  3




//#define RDEF 

#ifdef RDEF 
#include <R.h>
#define PRINTF Rprintf
#else 
#define PRINTF printf
#endif
 


struct generalizeOutput {
  unsigned int method;
  unsigned int length, max, val,sizeinbytes;
  
  TimeSeries *y;         // yes/no result             2-dim    
  TimeSeries *pvalue;    // confidence                2-dim
  TimeSeries *ym;        // model output              k-dim
  TimeSeries *dm;        // model variance            k-dim

};


typedef struct generalizeOutput GeneralizedOutput;

static inline void*  READ_STATE(int method, char ** x) {
  switch (method) {				
  case HoltWintersMethod:
    return (void *) ReadHWState(0,x);
    break; 
  case MovingAverageMethod:  
    return (void *) ReadMAState(0,x);
    break;
  case NonParametricMethod:  
  case KernelMethodMMD:  
    return (void*) ReadNONPState(0,x);
    break; 
  case MartingaleMethod:  
    return (void*) ReadMARPState(0,x);
    break; 
  case CompressionMethod:  
    return (void*) ReadKOLState(0,x);
    break; 
  default:
    return (void *)0;
    break; 
  }						
}


static  inline char *  WRITE_STATE(int method, void * x) {
  switch (method) {				
  case HoltWintersMethod:
    return PrintHWState((HoltWinterState *)x,0,0);
    break; 
  case MovingAverageMethod:  
    return PrintMAState((MovingAverageState *)x,0,0);
    break;
  case NonParametricMethod:  
  case KernelMethodMMD:  
    return PrintNONPState((NonParametricState *)x,0,0);
    break; 
  case MartingaleMethod:  
    return  PrintMartingaleState((MartingaleState *)x,0,0);
    break; 
  case CompressionMethod:  
    return  PrintCompressState((KolmogorovState *)x,0,0);
    break; 
  default:
    return (char *)0;
    break; 
  }						
}
  

static  inline void  FREE_STATE(int method, void * x) {	
  switch (method) {				
  case HoltWintersMethod:
    { 
      HoltWinterState * y = (HoltWinterState *)    x;
      FREE_HW_STATE(y); 
    }
    break; 
  case MovingAverageMethod:  
    { 
      MovingAverageState *y = (MovingAverageState *)    x;
      FREE_MA_STATE(y); 
    }
    break;
  case NonParametricMethod:  
  case KernelMethodMMD:  
    {
      NonParametricState * y = (NonParametricState*) x;
      FREE_NONP_STATE(y);
    }
    break; 
  case CompressionMethod:  
    {
      KolmogorovState * y = (KolmogorovState*) x;
      FREE_KOL_STATE(y);
    }
    break; 
  case MartingaleMethod:  
    {
      MartingaleState * y = (MartingaleState*) x;
      FREE_MARTINGALE_STATE(y);
    }
    break; 
  default:						break; 
  }						
}



#define ALLOCATE_GEN_OUTPUT(f,N,v,k) {					\
    assert(v>=2 &&  v<=4);						\
    (f) = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));	\
    assert(f);                                                          \
    switch (v) {                                                        \
      case 3:                                                           \
        ALLOCATETS((f)->ym,N,2);                                        \
        ALLOCATETS((f)->pvalue,N,2);                                    \
        ALLOCATETS((f)->y,N,2);                                         \
        break;                                                          \
      case 4:                                                           \
        ALLOCATETS((f)->dm,N,k);                                        \
        ALLOCATETS((f)->ym,N,k);                                        \
      case 2:                                                           \
        ALLOCATETS((f)->pvalue,N,2);                                    \
        ALLOCATETS((f)->y,N,2);                                         \
      default:                                                          \
      break;                                                            \
    }                                                                   \
    (f)->length=0;(f)->max=N;(f)->val = v;				\
  }

#define FREE_GEN_OUTPUT(f) {					\
    if (f) {							\
      FREETS((f)->pvalue);			                \
      FREETS((f)->y);			                        \
      FREETS((f)->ym);                                         \
      FREETS((f)->dm);                                         \
      free(f); f=0;						\
    }								\
  }





#ifdef RDEF
typedef int timestamp;
#else

typedef long int timestamp;;
#endif


#ifndef TIMESERIES_MODULE

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern int scalarF(TimeSeries *stream,
		   int Methods,
		   void **state,
		   GeneralizedOutput **out,
		   Mat *parameters);

extern int scalarFBASIC(timestamp *x, double *y, int *n, int *dimensions,
			int *method, 
			double *params, int *np,
			timestamp *xout, 
			double *yout, 
			double *pvalue, 
			double *ym, 
			double *dm, 
			int *outn,
			char **stateString); 



extern void PrintGeneralizedOutput(GeneralizedOutput *t);
extern int PrintGeneralizedOutputvertical(GeneralizedOutput *t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif



#endif









#endif
