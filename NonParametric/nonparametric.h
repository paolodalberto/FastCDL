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
#ifndef NONPARAMETRIC 
#define NONPARAMETRIC 

#include <window.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <quicksort.h>


//#define RDEF 

#ifdef RDEF 
#include <R.h>
#define PRINTF Rprintf
#else 
#define PRINTF printf
#endif


//#define PDFONLY 1
#define TOPOLOGICALONLY 1
//#define CDFCLASSICONLY

struct nonState { 
  unsigned int sizeinbytes;
  unsigned int code;
  Window *r;
  Window *w;
};


typedef struct nonState NonParametricState;


#define ALLOCATE_NONP_STATE(f,N,M,k) {					\
    (f) = (NonParametricState*) malloc(sizeof(NonParametricState));	\
    ALLOCATEW((f)->r,N,k);						\
    ALLOCATEW((f)->w,M,k);						\
    (f)->sizeinbytes = sizeof(NonParametricState);			\
    (f)->sizeinbytes += (f)->sizeinbytes%sizeof(Window);		\
    (f)->sizeinbytes += (f)->r->sizeinbytes;				\
    (f)->sizeinbytes += (f)->sizeinbytes%sizeof(Window);		\
    (f)->sizeinbytes += (f)->w->sizeinbytes;				\
     (f)->code= 1;\
  }






#define FREE_NONP_STATE(f) {						\
    if (f) {								\
      if ((f)->code ==1) { \
	/*PRINTF(" FREE NOP r\n");*/  FREEW((f)->r);	\
	/*PRINTF(" FREE NOP w\n "); */FREEW((f)->w);	\
      }\
      free(f); (f) = 0;							\
     									\
  }\
}



#define UPDATESTATE(state,stream,N,M) {			\
    if (!(state)) {					\
      ALLOCATE_NONP_STATE(state,N,M,stream->dimensions);			\
      (state)->r = WindowUpdate((state)->r,stream,N);	 \
      setCodeTimeseries((state)->r->ts,0);\
    }							\
    else {						\
      if ((state)->r->ts->length<N) {			\
	(state)->r = WindowUpdate((state)->r,stream,N);	\
        setCodeTimeseries((state)->r->ts,0);\
      } else /*if ((state)->w->ts->length<M)*/ {		\
	(state)->w = WindowUpdate((state)->w,stream,M);	\
        setCodeTimeseries((state)->w->ts,1);\
      }							\
    }							\
  }


#define PRINTNONPSTATE(t,i)         free(PrintNONPState(t,i, 1) )


#ifndef  NONPARAMETRIC_MODULE
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  extern int nonParametricDistance(TimeSeries *stream,
				   NonParametricState **state,
				   TimeSeries **val,
				   TimeSeries **pval,
				   int N, int M, int DF,
				   int type_order);
  
  extern char *PrintNONPState(NonParametricState *t, int identation, int P);
  extern NonParametricState *ReadNONPState(int identation, char **temp);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif



#endif
