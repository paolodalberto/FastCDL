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
#ifndef WINDOWINCLUDE
#define WINDOWINCLUDE 1 
#include <type.h>
#include <stdlib.h>
#include <stdio.h>


// Align  vectors and structure to the cache line 32 Bytes
#define MIN_ALIGN 32

#define SPRINT(dest,temp, S, A)  { sprintf(temp,S,A); strcat(dest,temp);}
#define SPRINTNA(dest,temp, S) { sprintf(temp,S); strcat(dest,temp);}


#define IDENT(N) {int j; for (j=0;j<N;j++) {printf(" ");}}
#define IDENTS(N,dest,temp)  {int j; for (j=0;j<N;j++) { SPRINTNA(dest,temp, (" "));}}

#define FINDSLASHN(temp,i) { for (i=0;(temp[i]!='\n' && temp[i]!= 0) ;i++);}
#define FINDCHAR(temp,c,i) { for (i=0;(temp[i]!=c    && temp[i]!= 0) ;i++);}


//#define RDEF 

#ifdef RDEF 
#include <R.h>
#define PRINTF Rprintf
#else 
#define PRINTF printf
#endif




#define TIMESLOT  0
#define VALUESLOT 1
#define COMPACT "Compact"
#define NONCOMPACT "Non Compact"

/***************************************************
 * Single Point data structure: x and y 
 * y size is specified by dimensions.
 * code is used to remember from which timeseries 
 */ 

struct sp {
  unsigned int dimensions; // dimension of the y array
  unsigned int code;     
  double cdf;     // used in the statistical window   
  double pdf;     
  Xorder x;                // time stamp
  Mat    *y;               // value
};

typedef struct sp SinglePoint; 


#define ALLOCATE_SP(f,k) {                               \
  f = (SinglePoint *) calloc(1,sizeof(SinglePoint));      \
  assert(f);                                             \
  f->y = (Mat *) calloc(k,sizeof(Mat)); f->dimensions =k;\
  assert(f->y);                                          \
}

static void FREE_SP(SinglePoint *f) { 
  if (f) {                              
     if (f->y) { free(f->y); f->y=0; }  
     free(f); f=0;                      
  }                                     
}

  /*
#define FREE_SP(f) {                    \
  if (f) {                              \
     if (f->y) { free(f->y); f->y=0; }  \
     free(f); f=0;                      \
  }                                     \
}
  */

#define PRINTSP(s) { free(PrintSP(&(s),0,1)); }

#ifdef GODSEESEVERYTHING
#define PRINTSP(s) {int _k;             \
   PRINTF("[ c=%d %ld,  (", ((s).code),((s).x));          \
/*   PRINTF("[ %ld,  (", ((s).x));  */        \
   for (_k=0;_k<(s).dimensions;_k++) {    \
       PRINTF("%e ",(s).y[_k]);           \
   }                                    \
   PRINTF(")");                       \
   if ((s).pdf) PRINTF(" pdf: %e",(s).pdf); \
   if ((s).cdf) PRINTF(" cdf: %e",(s).cdf); \
   PRINTF(" ]");                       \
}
#endif
#define SP_OP(d, equal, r,plus,alpha,multiply,l) {int _k;        \
   for (_k=0;_k<(r).dimensions;_k++) {                             \
       (d).y[_k] equal (r).y[_k] plus (alpha multiply (l).y[_k]);      \
   }                                                             \
}
#define SP_ADD(d, equal ,r,plus,l) {int _k;                      \
   for (_k=0;_k<(r).dimensions;_k++) {                           \
       (d).y[_k] equal (r).y[_k] plus (l).y[_k];                 \
   }                                                             \
}
#define SP_MUL(d, equal ,alpha, multiply,l) {int _k;                 \
   for (_k=0;_k<(l).dimensions;_k++) {                             \
       (d).y[_k] equal alpha multiply (l).y[_k];                         \
   }                                                             \
}


#define COPYSP(d,s) { Mat *_temp=(d).y;                              \
    (d) = (s);  (d).y = _temp; memcpy((d).y,(s).y,(s).dimensions*sizeof(Mat)); \
}

			     
/*************************************************
 * Series data structure 
 * max    = maximum lenght
 * length = current lenght 
 * codes are not used 
 * dimensions = dimensions of the points 
 * data  = array of single points 
 * data.y  = are actually a single block in memory
 */

struct ts { 
  unsigned int sizeinbytes;
  unsigned int max;
  unsigned int length;
  unsigned int code;
  unsigned int dimensions;
  Mat *data_anchor;


  SinglePoint *data;  

};

typedef struct ts TimeSeries; 





#define ALLOCATETS(f,N,k) {  int _i;				\
    (f)= (TimeSeries *) calloc(1,sizeof(TimeSeries));			\
    assert(f);								\
    (f)->length = 0; (f)->max =N;					\
    (f)->data = (SinglePoint *) calloc(N,sizeof(SinglePoint));		\
    assert(f->data); f->dimensions = k;  		                \
    (f)->data_anchor = (Mat *) calloc((N)*k,sizeof(Mat));			\
    assert((f)->data_anchor);                                           \
    for (_i=0;_i<N;_i++) {                                              \
	 f->data[_i].dimensions =k; f->data[_i].y =(f)->data_anchor+_i*(k);             \
    }                                                                   \
  }

#define FREETS(f) {				\
    if (f) {					\
      if ((f)->data && (f)->data_anchor) {        \
          free((f)->data_anchor);                 \
          free((f)->data);                      \
      }	                                        \
      free(f); (f)=0;				\
    }						\
  }
#define ALLOCATETS_TEMP(f,N,k) { 			\
    (f)= (TimeSeries *) calloc(1,sizeof(TimeSeries));			\
    assert(f);								\
    (f)->length = 0; (f)->max =N;					\
    (f)->data = (SinglePoint *) calloc(N,sizeof(SinglePoint));		\
    assert(f->data); f->dimensions = k;  		                \
      }

#define FREETS_TEMP(f) {			\
    if (f) {					\
      if ((f)->data ) {                         \
          free((f)->data);                      \
      }	                                        \
      free(f); (f)=0;				\
    }						\
  }

#define ASSIGN_SINGLEPOINT(t,p,s) {  Mat *_temp =  (t)->data[p].y;   \
 (t)->data[p] = *(s);  (t)->data[p].y = _temp;                \
 memcpy((t)->data[p].y,(s)->y, (s)->dimensions*sizeof(Mat)); \
}


#define COPY_TS(r, ts) {     int _i;              \
    for (_i=0;_i<ts->length;_i++) {               \
       ASSIGN_SINGLEPOINT((r),_i, (ts)->data+_i);     \
    }                                             \
    r->length=ts->length;                         \
    }

#define CROP_TS(r, ts,max) { int _i;            \
    for (_i=0;_i<max;_i++) {                    \
       ASSIGN_SINGLEPOINT((r),_i,(ts->data+_i));   \
    }                                           \
    (r)->length=max;                              \
    }

#define APPEND_TS(r, ts) { int _i;                       \
    for (_i=0; _i< (ts)->length; _i++)  {                  \
       ASSIGN_SINGLEPOINT((r),_i+r->length,(ts)->data +_i);  \
    }                                                    \
    r->length+=ts->length;                               \
    }                                                    

#define UPDATETS(t, ts,s) {                                 \
    if (ts) {                                               \
       ALLOCATETS(t,ts->length + 1,ts->dimensions);         \
       COPY_TS(t, ts );                                     \
    } else {                                                \
       ALLOCATETS(t,1,s->dimensions);                         \
    }                                                       \
    ASSIGN_SINGLEPOINT(t,t->max-1,s);                       \
    t->length= t->max;                                      \
    if (t->length>1) SORT_TIMESERIES(t,TIMESLOT);           \
}


#define UPDATETS_BY_VALUE(t, ts,s) {               \
    if (ts) {                                      \
       ALLOCATETS(t,ts->length + 1,ts->dimensions);\
       COPY_TS(t, ts );                            \
    } else {                                       \
       ALLOCATETS(t,1,s->dimensions);                \
    }                                              \
    ASSIGN_SINGLEPOINT(t,t->max-1,s);              \
    t->length= t->max;                             \
    if (t->length>1) SORT_TIMESERIES(t,VALUESLOT); \
}







/***********************************
 * Statistical time serier structure
 * pdf and df will be used for the extension of windows 
 * sortedts = sorted by value time series  
 *            the order is using the loop based order .... this is a strong order. 
 */



struct sw { 
  unsigned int sizeinbytes;
  int     statistics_len;
  int     statistics_len_df;

  TimeSeries *sortedts;

  double *pdf;
  double *df;
  double *ext_pdf;
  double *ext_df;
};

typedef struct sw StatisticalWindow; 


#define ALLOCATESW(sw,N,k) {						\
    (sw)= (StatisticalWindow *) calloc(1,sizeof(StatisticalWindow));	\
    assert(sw);								\
    ALLOCATETS((sw)->sortedts,N,k);					\
    (sw)->pdf =(double*) calloc(4*N,sizeof(double));			\
    assert((sw)->pdf);							\
    (sw)->df  = (sw)->pdf+N;						\
    (sw)->ext_pdf  = (sw)->pdf+2*N;					\
    (sw)->ext_df  = (sw)->pdf+3*N;					\
  }



#define FREESW(f) {							\
  if (f) {								\
    if ((f)->pdf) {							\
      free((f)->pdf);							\
      (f)->pdf= (f)->df = (f)->ext_df= (f)->ext_pdf =0;			\
    }									\
    FREETS((f)->sortedts);						\
    free(f); (f)=0;							\
  }									\
  }

/***************************
 * Circular buffer used for Holt-winters and moving average approach.
 * top = top of the buffer
 * length = lenght/max of the buffer
 */




struct mod { 
  unsigned int sizeinbytes;
  int top;
  unsigned int length;

  SinglePoint *array;
};

typedef struct mod CircularBuffer;

#define VALUE(f,i)  ((f)->array[((f)->top+i+(f)->length)%((f)->length)])

#define SHIFTADD(f,x) {				\
    COPYSP((f)->array[(f)->top],  x);           \
    (f)->top = (((f)->top+1)%((f)->length));	\
  }

#define BOTTOM(f)  ((f)->array[((f)->top+(f)->length)%((f)->length)])

#define ACCESSFROMBOTTOM(f,i) (f)->array[(((f)->top+i+(f)->length)%((f)->length))]

#define ALLOCATECIRCULAR(f,N,k) { Mat *_temp; int _i;      		\
    (f)= (CircularBuffer *) calloc(1,sizeof(CircularBuffer));		\
    assert(f);								\
    (f)->length = N;							\
    (f)->array = (SinglePoint *) calloc(N,sizeof(SinglePoint));		\
    assert((f)->array);							\
    (f)->top = 0;							\
    _temp = (Mat *) calloc(N*k,sizeof(Mat));			        \
    assert(_temp);                                                      \
    for (_i=0;_i<N;_i++) {                                              \
	 f->array[_i].dimensions =k; f->array[_i].y =_temp+_i*k;        \
    }                                                                   \
  }


#define FREECIRCULAR(f) {					\
    if (f) {							\
      if ((f)->array) {                                         \
         free((f)->array[0].y);                                 \
         free((f)->array);(f)->array =0;                        \
      } free(f); (f) = 0;					\
    }								\
  }




typedef struct adjacentmatrix Adj;

struct adjacentmatrix { 


  CircularBuffer *ts; // circular  buffer of size max (below)

  unsigned int max;
  unsigned int length;
  unsigned int top;

  double *adj; // circular buffer NxN        
  double *pdf;
  int    *mins;
  

};


#define ALLOCATE_ADJ(f,N,K) { f = allocate_adj(N,K); } 

/*
#define ALLOCATE_ADJ(f,N) { \
   (f) = (Adj*) calloc(1,sizeof(Adj));                        \
   assert(f);                                                 \
   ALLOCATECIRCULAR((f)->ts,N,ts->dimensions);               \
   (f)->max  = N;                                             \
   (f)->pdf  = (double*) calloc(N,sizeof(double));            \
   (f)->adj  = (double*) calloc(N*N,sizeof(double));          \
   (f)->mins = (int*) calloc(N,sizeof(int));                  \
   assert((f)->pdf && (f)->adj && (f)->mins);                 \
}
*/

#define FREEADJ(f) { \
   if (f) { \
     if ((f)->mins) { free((f)->mins); (f)->mins=0; }    \
     if ((f)->adj) { free((f)->adj); (f)->adj=0; }       \
     if ((f)->pdf) { free((f)->pdf); (f)->pdf=0; }       \
     if ((f)->ts) { FREECIRCULAR((f)->ts); (f)->ts =0; } \
     free(f); (f)=0;                                     \
   }                                                     \
}




typedef struct w Window; 
#include <poset.h>

/*************************************
 * A Window 
 * ts = original series 
 * sw = statistical time series see above
 * poset = partial order by value of the sw->ts 
 *         this will be used for the distibution function, which is based on a weak order
 */ 


struct w { 
  unsigned int sizeinbytes;
  char comment[32];
  unsigned int code;
  TimeSeries        *ts;
  StatisticalWindow *sw;
  int posetlen;
  PartialOrder      **poset; 
  Chain_Heads       *topologicalsort;
};


#define ALLOCATEW(w,N,k) (w) = allocatew((N),(k))						
/*
#define ALLOCATEW(w,N,k) {						\
    (w)= (Window *) calloc(1,sizeof(Window));				\
    assert(w);								\
    strcpy((w)->comment,NONCOMPACT);					\
    ALLOCATETS((w)->ts,N,k);						\
    ALLOCATESW((w)->sw,N,k);						\
    ALLOCATE_POSET((w)->poset,N);						\
  }
*/
#define ALLOCATE_AND_SETW(w,ts) {					\
    (w) = WindowFromTimeSeries(w,ts);					\
  }

#define FREEW(f)  {		\
    if (f) {			\
      if ((f)->poset) { FREE_POSET((f)->poset,(f)->posetlen); free((f)->poset); (f)->poset =0;   }\
      if ((f)->topologicalsort) { FREE_CHAIN((f)->topologicalsort); }\
      FREESW((f)->sw);		\
      FREETS((f)->ts);		\
      free(f); (f) = 0;		\
    }				\
  }

#define FREE_EW(f,g)  {							\
    if (g) {								\
      if ((g)->sw) {							\
	if ((g)->sw->pdf) { free((g)->sw->pdf); (g)->sw->pdf=0;}	\
	free(g->sw); g->sw=0;						\
      }									\
      free(g); g=0;							\
    }									\
    if (f) {								\
      FREESW((f)->sw);							\
      free(f); (f) = 0;							\
    }									\
  }


#define PRINTWINDOW(t,i)            free(PrintWindow(t,i,1))
#define PRINTTIMESERIES(t,i)        free(PrintTimeseries(t,i,1))
#define PRINTSTATISTICALWINDOW(t,i) free(PrintStatisticalWindow(t,i,1))
#define PRINTCIRCULAR(t,i)          free(PrintCircular(t,i,1))

#define READTIMESERIES(t)        ReadTimeseries(0,&t)


#ifndef WINDOW_MODULE
#if defined(c_plusplus) || defined(__cplusplus)
 extern "C" {
#endif
   
   extern Window *WindowFromTimeSeries(TimeSeries *ts);
   extern Window *WindowUpdate(Window *r, TimeSeries *ts,int max); 
   extern double euclidean_distance(SinglePoint r, SinglePoint l);
   extern SinglePoint SP_add(SinglePoint *res, SinglePoint l, SinglePoint r);
   extern SinglePoint SP_sub(SinglePoint *res, SinglePoint l, SinglePoint r);
   extern SinglePoint SP_mul(SinglePoint *res, Mat l, SinglePoint r);
   extern SinglePoint SP_abs(SinglePoint *res, SinglePoint r);
   extern SinglePoint SP_min(SinglePoint *res, SinglePoint l, SinglePoint r);

   extern void append_ts(TimeSeries *r, TimeSeries *ts);
   extern void copy_ts(TimeSeries *r, TimeSeries *ts);
   extern void copy_ts_b(TimeSeries *r, TimeSeries *ts, int from, int to);
   extern Window *allocatew(unsigned int N, unsigned int k); 
   extern char * PrintSP(SinglePoint *t, int identation, int P);
   extern SinglePoint * readSP(int identation, char **temp);

   
   extern char *PrintWindow(Window *t,int i, int P);
   extern char *PrintTimeseries(TimeSeries *t,int i, int P);
   extern char *PrintStatisticalWindow(StatisticalWindow *t,int i,int P);
   extern char *PrintCircular(CircularBuffer *t,int i, int P);
   
   extern TimeSeries *ReadTimeseries(int identation, char **temp);
   extern StatisticalWindow *ReadStatisticalWindow(int identation, char *temp);
   extern Window * ReadWindow(int identation, char **temp);
   extern CircularBuffer *ReadCircular(int identation,char **temp);

   extern void setCodeTimeseries(  TimeSeries *ts, int code);
   extern Adj *allocate_adj(int N, int dimensions);
   
#if defined(c_plusplus) || defined(__cplusplus)
 }
#endif
 
#endif
 






#endif
