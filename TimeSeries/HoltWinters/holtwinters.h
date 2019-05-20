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
#ifndef HOLTWINTERSINTERFACE
#define HOLTWINTERSINTERFACE


#include<window.h>


struct stateHW { 
  Mat alpha, beta, gamma; 
  SinglePoint *yt, *at, *bt, *dt;  
  unsigned int sizeinbytes;
  unsigned int code;
  CircularBuffer *c;
  CircularBuffer *d;
};

typedef struct stateHW HoltWinterState;

#define ALLOCATE_HW_STATE(f,N,a,b,g,k) {					\
    (f) = (HoltWinterState *) calloc(1,sizeof(HoltWinterState));		\
    assert(f);								\
    (f)-> code  = 1;\
    ALLOCATE_SP((f)->yt,k); ALLOCATE_SP((f)->at,k);                           \
    ALLOCATE_SP((f)->bt,k); ALLOCATE_SP((f)->dt,k);                           \
    if (N>0) { ALLOCATECIRCULAR((f)->c,N,k);} else (f)->c=0;		\
    if (N>0) { ALLOCATECIRCULAR((f)->d,N,k);} else (f)->d=0;		\
    /*(f)->yt= (f)->at = (f)->bt = 0; f->code=1; (f)->dt=0;	*/	\
    (f)->alpha =a; (f)->beta=b; (f)->gamma=g;				\
  } 




#define FREE_HW_STATE(f) {					\
    if (f) {							\
      if ((f)->code) {                                            \
        FREECIRCULAR((f)->d);					\
        FREECIRCULAR((f)->c);                                   \
        FREE_SP((f)->yt); FREE_SP((f)->at);                     \
        FREE_SP((f)->bt);FREE_SP((f)->dt);                      \
        (f)->code =0;\
      }					                        \
      free(f); (f)=0;						\
    }								\
} 


#define PRINTHWSTATE(t,i)         free(PrintHWState(t,i,1))


#ifndef HOLTWINTERS_MODULE

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  extern int HoltWinters(TimeSeries *stream,
			 HoltWinterState **state,
			 TimeSeries **ytmOut,
			 TimeSeries **dtmOut,
			 int P, 
			 Mat alpha,
			 Mat beta,
			 Mat gamma);
  
  // these are for serialization and printing 
  extern char * PrintHWState(HoltWinterState *t, int identation, int P);
  extern HoltWinterState * ReadHWState(int identation, char **temp);
  //  extern void FREE_HW_STATE(HoltWinterState *f);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif








#endif
