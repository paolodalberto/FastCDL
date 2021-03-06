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
#ifndef EMPIRICAL_KOLMOGOROV_INFORMATION
#define EMPIRICAL_KOLMOGOROV_INFORMATION

#include <nonparametric.h>
#include <pvalue.h>
#include <window.h>

typedef unsigned int (*Compression) (char *, unsigned int );

struct kolmogorovState { 
  NonParametricState *s;
  unsigned int BMAX;
  char *r; unsigned int lr;
  char *w; unsigned int lw;
  int baseline;
  double boundaries[COL];
};

typedef struct kolmogorovState KolmogorovState;    



#define ALLOCATE_KOL_STATE(f,N,M,k) {					\
    (f) = (KolmogorovState*) calloc(1,sizeof(KolmogorovState));	\
    ALLOCATE_NONP_STATE((f)->s,N,M,k);\
}

#define FREE_KOL_STATE(f) {						\
    if (f) {								\
      FREE_NONP_STATE((f)->s);\
      /*free((f)->r); (f)->r=0;*/		\
      /*free((f)->w); (f)->w=0;*/		\
      free(f); (f) = 0;							\
  }\
}



#define UPDATEKOLSTATE(state,stream,N,M,BOOTMAX) {			\
    if (!(state)) {				 	        \
      ALLOCATE_KOL_STATE(state,N,M,stream->dimensions);                            \
      UPDATESTATE((state)->s,stream,N,M);                       \
      /*(state)->r = PrintTimeSeriesY((state)->s->r->ts,0,0);*/     \
      /*(state)->lr = strlen(state->r); */                          \
      (state)->r = (char *)(state)->s->r->ts->data_anchor;     \
      (state)->lr = state->s->r->ts->length*stream->dimensions*sizeof(Mat);                           \
      (state)->BMAX = BOOTMAX;\
    }							        \
    else { UPDATESTATE((state)->s,stream,N,M);                  \
         if ((state)->s->r->ts->length<N) {			\
            if ((state)->r) free((state)->r);                   \
             /*(state)->r = PrintTimeSeriesY((state)->s->r->ts,0,0);*/     \
             /*(state)->lr = strlen(state->r); */                          \
             (state)->r = (char*)(state)->s->r->ts->data_anchor;     \
             (state)->lr = state->s->r->ts->length*stream->dimensions*sizeof(Mat);                           \
         } else /*if ((state)->w->ts->length<M)*/ {		\
            if ((state)->w) free((state)->w);                   \
            /*(state)->w = PrintTimeSeriesY((state)->s->w->ts,0,0);*/\
            /* (state)->lw = strlen(state->w);                    */ \
            (state)->w = (char *)(state)->s->w->ts->data_anchor;\
            (state)->lw = state->s->w->ts->length*stream->dimensions*sizeof(Mat);                     \
         }							\
    }							        \
  }


#define PRINTKOLSTATE(t,i)         { free(PrintCompressState(t,i, 1) ); } 




#ifndef COMPRESS_MODULE


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


  extern double normalized_compression_difference_measure(char *r, unsigned int rlen, 
							  char *c, unsigned int clen, 
							  Compression *comp );
  extern unsigned int zlib_compress( char *string, unsigned int len); 
  extern char * PrintCompressState(KolmogorovState *s, int indentation, int P) ;


  extern int compressionDistance(TimeSeries *stream,
				 KolmogorovState **state,
				 TimeSeries **val,TimeSeries **pval,
				 int N, int M, int DF, int BM);
  extern KolmogorovState * ReadKOLState(int identation, char **temp) ;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif





#endif








#endif
