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
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <interface.h>
#include <string.h>
#include <assert.h>


#include <window.h>
#include <sort.h>
#include <quicksort.h>

#include <doubly_compensated_sumc.h>

static int debug =0;


#ifndef MIN
#define MAX(x,y) ((x<y)?y:x)
#define MIN(x,y) ((x<y)?x:y)
#endif

void bottom(SinglePoint *m, SinglePoint *tem) { 
  int i;
  int n = MIN(m->dimensions,tem->dimensions); 
  for (i=0;i<n ;i++) 
    m->y[i] = MIN(m->y[i],tem->y[i]);

}
void top(SinglePoint *m, SinglePoint *tem) { 
  int i;
  int n = MIN(m->dimensions,tem->dimensions); 
  for (i=0;i<n ;i++) 
    m->y[i] = MAX(m->y[i],tem->y[i]);

}

int GT(SinglePoint x, SinglePoint y) {  // ((x) > (y))
  int i=0,b=0,c=0;
  int m = MIN(x.dimensions,y.dimensions); 
  
  do { 
    b = x.y[i] >= y.y[i];
    c += (x.y[i] > y.y[i])?1:0;
	
  } while (b && (++i)<m); 
  
  return (c)?i:0;
}
int LT(SinglePoint x, SinglePoint y) {  // ((x) < (y))
  int i=0,b=0,c=0;
  int m = MIN(x.dimensions,y.dimensions); 
  
  do { 
    b = x.y[i] <= y.y[i];
    c += (x.y[i] < y.y[i])?1:0;
  } while (b && (++i)<m); 

  return (c)?i:0;
}

int GE(SinglePoint x, SinglePoint y) {  // ((x) >= (y))
  int i=0,b=0;
  int m = MIN(x.dimensions,y.dimensions); 
  
  do { 
    b = x.y[i] >= y.y[i];

  } while (b && (++i)<m); 

  return i;
}

int LE(SinglePoint x, SinglePoint y) {  // ((x) <= (y))
  int i=0,b=0;
  int m = MIN(x.dimensions,y.dimensions); 
  
  do { 
    b = x.y[i] <= y.y[i];

  } while (b && (++i)<m); 

  return i;
}

int EQ(SinglePoint x, SinglePoint y) {  // ((x) == (y))


  int i=0,b=0;
  int m = MIN(x.dimensions,y.dimensions); 
  
  do { 
    b = x.y[i] == y.y[i];

  } while (b && (++i)<m); 

  return b;
}

int NE(SinglePoint x, SinglePoint y) {  // ((x) != (y))

  return !EQ(x,y);

}







#define DECREASING_ORDER 1

#if(DECREASING_ORDER) 
#define BB(i,j,src,pivot,lower,upper) {                         \
    do (i)++;                                                   \
    while ((i)<=(upper) && GTX(ABS(((src)[(i)])), ABS(pivot)));	\
    do (j)--;                                                   \
    while ((j>=lower) && LTX(ABS((src)[(j)]), ABS(pivot)));      \
  }
#define BBI(i,j,src,pivot,lower,upper) {                          \
    do (i)++;                                                     \
    while ((i)<=(upper) && LTX(ABS(((src)[(i)])), ABS(pivot)));	  \
    do (j)--;                                                     \
    while ((j>=lower) && GTX(ABS((src)[(j)]), ABS(pivot)));       \
  }
#else 
#define BB(i,j,src,pivot,lower,upper) {                         \
    do (i)++;                                                   \
    while ((i)<=(upper) && LTX(ABS((src)[(i)]), ABS(pivot)));   \
    do (j)--;                                                   \
    while ((j>=lower) && GTX(ABS((src)[(j)]), ABS(pivot)));     \
  }
#define BBI(i,j,src,pivot,lower,upper) {                      \
    do (i)++;                                                 \
    while ((i)<=(upper) && GTX(ABS((src)[(i)]), ABS(pivot))); \
    do (j)--;                                                 \
    while ((j>=lower) && LTX(ABS((src)[(j)]), ABS(pivot)));   \
  }
#endif

#if(DECREASING_ORDER) 
#define BBW(i,j,src,pivot,lower,upper) {                                                                               \
    do { (i)++; if (debug) printf(" s i %d %s > %s %d\n",i,src->word[i],pivot.word,GTW(src->word[(i)], pivot.word));}  \
    while ((i)<=(upper) && GTW(src->word[(i)], pivot.word));	                                                       \
    do {(j)--; if (debug) printf(" s j %d %s < %s %d\n\n",j,src->word[j],pivot.word,LTW((src->word[(j)]), pivot.word));} \
    while ( (j>=lower) && LTW((src->word[(j)]), pivot.word));                                                          \
  }
#else 
#define BBW(i,j,src,pivot,lower,upper) {                                                                                   \
    do {(i)++; if (debug) printf(" s i %d %s < %s %d\n",i,src->word[i],pivot.word,LTW(src->word[(i)], pivot.word));}       \
    while ((i)<=(upper) && LTW((src->word[(i)]), pivot.word));                                                             \
    do {(j)--; if (debug) printf(" s j %d %s > %s %d\n\n",j,src->word[j],pivot.word,GTW((src->word[(j)]), pivot.word));}   \
    while ((j>=lower) && GTW((src->word[(j)]), pivot.word) );                                                              \
  }
#endif
#if(DECREASING_ORDER) 
#define BBT(i,j,src,pivot,lower,upper,key) { if (debug) {PRINTF(" DECREASING \n"); }		                                                                                           \
    do { (i)++; if (debug && (i)<=(upper)) {PRINTSP(src->data[i]); PRINTF(">"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",GTT(src,i,pivot,0,key));}}\
    while ((i)<=(upper) && GTT(src,i,pivot,0,key));	                                                                                           \
    do {(j)--; if (debug &&(j>lower)) {PRINTSP(src->data[j]); PRINTF("<"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",LTT(src,j,pivot,0,key));}}     \
    while ( (j>=lower) && LTT(src,j,pivot,0,key));                                                                                                 \
  }
#else 
#define BBT(i,j,src,pivot,lower,upper,key) {		                                                                                           \
    do {(i)++; if (debug && (i)<=(upper)) {PRINTSP(src->data[i]); PRINTF("<"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",LTT(src,i,pivot,0,key));}} \
    while ((i)<=(upper) && LTT(src,i,pivot,0,key));	                                                                                           \
    do {(j)--; if (debug && (j>lower)) {PRINTSP(src->data[j]); PRINTF(">"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",GTT(src,j,pivot,0,key));}}    \
    while ((j>=lower) && GTT(src,j,pivot,0,key) );                                                                                                 \
  }
#endif
#if(DECREASING_ORDER) 
#define BBET(i,j,src,pivot,lower,upper,key) { if (debug) {PRINTF(" DECREASING \n"); }		                                                                                           \
    do { (i)++; if (debug && (i)<=(upper)) {PRINTSP(src->data[i]); PRINTF(">"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",GTT(src,i,pivot,0,key));}}\
    while ((i)<=(upper) && GET(src,i,pivot,0,key));	                                                                                           \
    do {(j)--; if (debug &&(j>lower)) {PRINTSP(src->data[j]); PRINTF("<"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",LTT(src,j,pivot,0,key));}}     \
    while ( (j>=lower) && LET(src,j,pivot,0,key));                                                                                                 \
  }
#else 
#define BBET(i,j,src,pivot,lower,upper,key) {		                                                                                           \
    do {(i)++; if (debug && (i)<=(upper)) {PRINTSP(src->data[i]); PRINTF("<"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",LTT(src,i,pivot,0,key));}} \
    while ((i)<=(upper) && LET(src,i,pivot,0,key));	                                                                                           \
    do {(j)--; if (debug && (j>lower)) {PRINTSP(src->data[j]); PRINTF(">"); PRINTSP(pivot->data[0]); PRINTF("T %d\n",GTT(src,j,pivot,0,key));}}    \
    while ((j>=lower) && GET(src,j,pivot,0,key) );                                                                                                 \
  }
#endif


#ifndef CUTOFF
#define CUTOFF 4
#endif

void print(Mat *m, int n){ 
  int i;
  for (i=0;i<n;i++) 
    printf("%e ",fabs(m[i]));

  printf("\n");


}



void  insort (register Mat *array, register int len)
{
        register int    i, j;
        register Mat  temp;
	if (debug) {
	  printf("insort\n") ;
	  print(array,len);
	}
        for (i = 1; i < len; i++) {
                /* invariant:  array[0..i-1] is sorted */
                j = i;
                /* customization bug: SWAP is not used here */
                temp = array[j];
#if(DECREASING)
                while (j > 0 && GTX(ABS(array[j-1]), ABS(temp)))
#else
                while (j > 0 && LTX(ABS(array[j-1]), ABS(temp)))
#endif
		  {
                        array[j] = array[j-1];
                        j--;
                }
                array[j] = temp;
		if (debug) { 
		  printf("i %d j %d ",i,j);
		  print(array,len);
		}
        }
	if (debug) { 
	  printf("insort final\n");
	  print(array,len);
	  printf("\n");
	}
}



void  partial_quickersort (register Mat * array, register int lower, register int upper)
{
    register int        i, j;
    register Mat        pivot;

    if (debug) { 
      printf(" quick sort %d\n",upper-lower+1);
      print(array+lower,upper-lower+1);

    }
    if (upper - lower > CUTOFF) {
        SWAP(array[lower], array[(upper+lower)/2]);
        i = lower;  j = upper+1;  pivot = array[lower];
        while (1) {
	  BB(i,j,array,pivot,lower,upper);
	  if (j < i) break;
	  if (debug) printf("swap %d %d\n",i, j);
	  SWAP(array[i], array[j]);
        }
	if (debug) printf("swap %d %d j %d i\n",lower, j,i);
	SWAP(array[lower], array[j]);
	
	if (debug) { printf("split \n"); print(array+lower,upper-lower+1);} 
        partial_quickersort (array, lower, j - 1);
	if (debug) print(array+lower,j-lower);
        partial_quickersort (array, i, upper);
        if (debug) print(array+i,upper-i+1);

    }
    else { 
      insort( array+lower,upper - lower+1);

    }
    if (debug) {printf("result quicksort\n"); print(array+lower,upper-lower+1); printf("\n");}

}

void  insort_inverse (register Mat *array, register int len)
{
        register int    i, j;
        register Mat  temp;

        for (i = 1; i < len; i++) {
                /* invariant:  array[0..i-1] is sorted */
                j = i;
                /* customization bug: SWAP is not used here */
                temp = array[j];
	       
#if(DECREASING)	       
                while (j > 0 && GTX(ABS(array[j-1]), ABS(temp))) 
#else
		while (j > 0 && LTX(ABS(array[j-1]), ABS(temp))) 
		    
#endif
		  {
		    array[j] = array[j-1];
		    j--;
                }
                array[j] = temp;
        }
}



void  partial_quickersort_inverse (register Mat * array, register int lower, register int upper)
{
    register int        i, j;
    register Mat        pivot;
    if (upper - lower > CUTOFF) {
        SWAP(array[lower], array[(upper+lower)/2]);
        i = lower;  j = upper+1;  pivot = array[lower];
        while (1) {
	  BBI(i,j,array,pivot,lower,upper);
	  if (j < i) break;
	  SWAP(array[i], array[j]);
        }
        SWAP(array[lower], array[j]);
        partial_quickersort_inverse (array, lower, j - 1);
        partial_quickersort_inverse (array, i, upper);
    }
    else { 
      insort_inverse( array+lower,upper - lower+1);

    }
}



void  insort_histogram (Histogram *array, register int lower, register int upper)
{
  register int    i, j;
  Singleton temp;
  if (debug) { 
    printf("insort %d %d\n",lower,upper);
    printHistogram(array);
    
  }
  
  for (i = lower; i <= upper; i++) {
    /* invariant:  array[0..i-1] is sorted */
    j = i;
    /* customization bug: SWAP is not used here */
    if (debug) { 
      printf("%d ",i);
      printf("%s %d\n ", array->word[j],(int)array->number[j]);

     }
    temp.word   = array->word[j];
    temp.number = array->number[j];
 	       
    while (j > lower && 
#if(DECREASING_ORDER) 
	   LTW(array->word[j-1], temp.word)
#else
	   GTW(array->word[j-1], temp.word)
#endif	   
	   ) {
	if (debug) { 
	  printf("W j=%d %s %d <- %s %d", j,array->word[j],(int)array->number[j],array->word[j-1],(int)array->number[j-1]);
	  
	}
	array->word[j]   = array->word[j-1];
	array->number[j] = array->number[j-1];
	j--;
      }
    array->word[j]   = temp.word;
    array->number[j] = temp.number;
  }
}





void  sorthistogram (register Histogram * array, register int lower, register int upper)
{
    register int        i, j;
    Singleton  pivot;
    if (debug) { 
      printf("sorthistogram %d %d \n",lower,upper);
      printHistogram(array);
      
    }

    if (upper - lower > CUTOFF) {
      if (debug) { 
	printf("upper %d - lower %d> CUTOFF \n",lower,upper);
      }
      

      SWAPH(array,lower, (upper+lower)/2);
      i = lower;  j = upper+1;  
      pivot.word   = array->word[lower];
      pivot.number = array->number[lower];
      
      while (1) {
	BBW(i,j,array,pivot,lower,upper);
	if (j < i) break;
	SWAPH(array,i,j);
      }
      SWAPH(array,lower,j);
      sorthistogram(array, lower, j-1);
      sorthistogram(array, i, upper);
    }
    else { 
      insort_histogram(array,lower,upper);
      
    }
}


// assuming the histograms have been sorted
void extendHistogram(Histogram *inh1, Histogram *inh2,
		     Histogram *ouh1, Histogram *ouh2) {


  int N = inh1->length+inh2->length;
  int i,j,k; 

  
  ouh1->word   = (char **) calloc(N,sizeof(char**));
  ouh1->number = (Mat*) calloc(N,sizeof(Mat));
  ouh2->word   = (char **) calloc(N,sizeof(char**));
  ouh2->number = (Mat*) calloc(N,sizeof(Mat));
  assert(ouh1->word && ouh1->number &&ouh2->word &&ouh2->number);

  i = j = k=0;

  while (i<inh1->length && j<inh2->length) { 
 
if 
#if(DECREASING_ORDER)    
  (GTW(inh1->word[i],inh2->word[j])) 
#else
  (LTW(inh1->word[i],inh2->word[j])) 
#endif
    {
      ouh1->word[k]    =inh1->word[i]; 
      ouh1->number[k]  =inh1->number[i]; 
      ouh2->word[k]   =inh1->word[i]; 
      ouh2->number[k]  = 0;
      i++;
    }
 else 
   if
#if(DECREASING_ORDER)    
     (LTW(inh1->word[i],inh2->word[j])) 
#else
     (GTW(inh1->word[i],inh2->word[j])) 
#endif
       { 
	 ouh1->word[k]    =inh2->word[j]; 
	 ouh1->number[k]  = 0;
	 ouh2->word[k]   = inh2->word[j]; 
	 ouh2->number[k]  = inh2->number[j]; 
	 j++;
       }
   else 
     { 
       ouh1->word[k]    = inh1->word[i]; 
       ouh1->number[k]  = inh1->number[i]; 
       ouh2->word[k]   = inh2->word[j]; 
       ouh2->number[k]  = inh2->number[j]; 
       j++;
       i++;
     }
   k++;
 
  }

  while (i<inh1->length) { 
      ouh1->word[k]    =inh1->word[i]; 
      ouh1->number[k]  =inh1->number[i]; 
      ouh2->word[k]   =inh1->word[i]; 
      ouh2->number[k]  = 0;
      i++;
      k++;
  }
  while (j<inh2->length) { 
    ouh1->word[k]    =inh2->word[j]; 
    ouh1->number[k]  = 0;
    ouh2->word[k]   = inh2->word[j]; 
    ouh2->number[k]  = inh2->number[j]; 
    j++;
    k++;
  }
  
  ouh1->length = k;
  ouh2->length = k;
  
}
// assuming the histograms have been sorted
void UnionIntersectionHistogram(Histogram *inh1, Histogram *inh2,
				Histogram *un, Histogram *in) {


  int N = inh1->length+inh2->length;
  int i,j,k,l; 

  
  un->word   = (char **) calloc(N,sizeof(char**));
  un->number = (Mat*) calloc(N,sizeof(Mat));
  in->word   = (char **) calloc(N,sizeof(char**));
  in->number = (Mat*) calloc(N,sizeof(Mat));
  assert(un->word &&  un->number && in->word && in->number);

  i = j = k=l=0;

  while (i<inh1->length && j<inh2->length) { 
 
if 
#if(DECREASING_ORDER)    
  (GTW(inh1->word[i],inh2->word[j])) 
#else
  (LTW(inh1->word[i],inh2->word[j])) 
#endif
    {
      un->word[k]    =inh1->word[i]; 
      un->number[k]  =inh1->number[i]; 
      i++;
    }
 else 
   if
#if(DECREASING_ORDER)    
     (LTW(inh1->word[i],inh2->word[j])) 
#else
     (GTW(inh1->word[i],inh2->word[j])) 
#endif
       { 
	 un->word[k]   = inh2->word[j]; 
	 un->number[k]  = inh2->number[j]; 
	 j++;
       }
   else 
     { 
       un->word[k]    = inh1->word[i]; 
       un->number[k]  = MAX(inh1->number[i],inh2->number[j]); 
       in->word[l]    = inh1->word[i]; 
       in->number[l]  = MIN(inh1->number[i],inh2->number[j]); 
       l++;
       j++;
       i++;
     }
   k++;
 
  }

  while (i<inh1->length) { 
      un->word[k]    =inh1->word[i]; 
      un->number[k]  =inh1->number[i]; 
      i++;
      k++;
  }
  while (j<inh2->length) { 
    un->word[k]   = inh2->word[j]; 
    un->number[k]  = inh2->number[j]; 
    j++;
    k++;
  }
  
  un->length = k;
  in->length = l;
  
}



void  insortTimeSeries (TimeSeries *array, register int lower, register int upper, int key)
{
  register int i, j;
  TimeSeries *temp;


  ALLOCATETS_TEMP(temp,1,array->dimensions);
  
  if (debug) { 
    printf("to sort %d %d %d\n",lower,upper,key);
    PRINTTIMESERIES(array,0);
    printf("\n") ;
  }
  
  for (i = lower; i <= upper; i++) {
    /* invariant:  array[0..i-1] is sorted */
    j = i;
    /* customization bug: SWAP is not used here */
    temp->data[0]   = array->data[j];
    if (debug) { 
      printf("%d ",i);
      PRINTSP(array->data[i]);
            
    }

    while (j >lower  && 
#if(DECREASING_ORDER) 
	   LTT(array,j-1,temp,0,key)
#else
	   GTT(array,j-1,temp,0,key)
#endif	   
) {
      array->data[j]   = array->data[j-1];
      j--;
    }
    array->data[j]   = temp->data[0];
  }

  if (debug) { 
    printf("sorted %d %d %d\n",lower,upper,key);

    PRINTTIMESERIES(array,0);    
  }

  

  FREETS_TEMP(temp);
}




int verifyT(register TimeSeries * array, int key) { 
  int i;
  int e=1;

  for (i=1;i<array->length;i++) { 
    if ( !
#if(DECREASING_ORDER) 
	LET(array,i,array,i-1,key)
#else
	GET(array,i,array,i-1,key)
#endif	
	) 
      { 
	PRINTSP(array->data[i-1]); PRINTSP(array->data[i]);
	printf("failed comparison at %d \n",i);
	e = 0;


      }

 
  }
  printf("\n");

  return e;
}


void  sortTimeSeries (register TimeSeries * array, register int lower, register int upper,int key)
{
  register int        i, j;
  TimeSeries *pivot;


  if (debug) { 
    printf("sort %d %d %d\n",lower,upper,key);

    PRINTTIMESERIES(array,0);
    printf("\n") ;
  }

  if (upper - lower > CUTOFF) {
    SWAPT(array,lower, (upper+lower)/2);
    i = lower;  j = upper+1;  

    ALLOCATETS(pivot,1,array->dimensions);
    
    COPYSP(pivot->data[0],array->data[lower])
    
    while (1) {
      BBT(i,j,array,pivot,lower,upper,key);
      if (j <= i) break;
      if (debug) { 
	PRINTSP(array->data[i]); PRINTSP(array->data[j]); 
	printf(" SWAPPED \n");
      }
      SWAPT(array,i,j);
    }

    FREETS(pivot);

    
    SWAPT(array,lower,j);
    sortTimeSeries(array, lower, j - 1,key);
    sortTimeSeries(array, i, upper,key);
    
    if (debug) { 
      printf("sorted %d %d %d\n",lower,upper,key);
      
      PRINTTIMESERIES(array,0);
      printf("\n") ;
    }

  }
  else { 
    if (upper-lower>=1)
      insortTimeSeries(array,lower,upper,key);
  }


}

/************
 * This is obsolete in the multidimensional env
 *
 *
 *


// assuming the histograms have been sorted
void extendWindow(Window *inh1, Window *inh2,
		  Window *ouh1, Window *ouh2, int key) {

  StatisticalWindow *swo1, *swo2;
  TimeSeries *rts,*wts,*sts;

  int N = inh1->ts->length+inh2->ts->length;
  int NS = inh1->sw->sortedts->length+inh2->sw->sortedts->length;
  int i,j,k;
 

  if (debug) { 
    printf("N %d NS %d\n",N,NS);
    printf("in 1 :\n");
    PRINTWINDOW(inh1,3);
    printf("in 2:\n");
    PRINTWINDOW(inh1,3);
  }

  ouh1->ts = inh1->ts;
  ouh2->ts = inh2->ts;

  // the statistical windows share teh sorted input timeseries

  swo1 = swo2=0;
  ALLOCATESW(swo1,NS, inh1->ts->dimensions);

  swo2= (StatisticalWindow *) malloc(sizeof(StatisticalWindow));		
  assert(swo2);								
  swo2->pdf =(double*) calloc(2*N,sizeof(double));			
  assert(swo2->pdf);							
  swo2->df  = swo2->pdf+N;							
  swo2 -> sortedts = swo1 ->sortedts; 
  swo2->sizeinbytes = 0;
  ouh1->sw = swo1;
  ouh2->sw = swo2;

  if (debug) { 

    printf("swo1 \n");
    PRINTSTATISTICALWINDOW(swo1,3);
    printf("swo2:\n");
    PRINTSTATISTICALWINDOW(swo2,3);
  }
  
  
  rts = inh1->sw->sortedts;
  wts = inh2->sw->sortedts;
  sts = swo1->sortedts;
   
  if (debug) { 
    printf("rts \n");
    PRINTTIMESERIES(rts,3);
    printf("wts \n");
    PRINTTIMESERIES(wts,3);
    printf("sts \n");
    PRINTTIMESERIES(sts,3);
  }


  i = j = k=0;
  if (debug) { 
    printf("Start to shuffle %d %d %d \n",i,j,k);
   }

  while (i<rts->length && j<wts->length) { 
   if (debug) { 
     PRINTSP(rts->dataprintf("%f %f \n",rts->y[i],wts->y[j]);
   }
   
    if 
#if(DECREASING_ORDER)    
      (GTT(rts,i,wts,j,key)) 
#else
      (LTT(rts,i,wts,j,key)) 
#endif
	{
	  if (debug) printf("A \n");
	  sts->x[k]    = rts->x[i];
	  sts->y[k]    = rts->y[i];
	  swo1->pdf[k] =inh1->sw->pdf[i]; 
	  swo2->pdf[k] = 0;
	  i++;
	}
    else 
      if
#if(DECREASING_ORDER)    
	(LTT(rts,i,wts,j,key)) 
#else
	(GTT(rts,i,wts,j,key)) 
#endif
	  { 
	    if (debug) printf("B \n");
	    sts->x[k]    = wts->x[j];
	    sts->y[k]    = wts->y[j];
	    swo1->pdf[k] = 0;
	    swo2->pdf[k] = inh2->sw->pdf[j]; 
	    j++;
	  }
      else 
	{ 
	  if (debug) printf("C\n");
	  sts->x[k]    = wts->x[j];
	  sts->y[k]    = wts->y[j];
	  swo1->pdf[k] = inh1->sw->pdf[i];
	  swo2->pdf[k] = inh2->sw->pdf[j]; 
	  j++;
	  i++;
	}
    k++;
    if (debug) { 
      printf(" K %d \n",k);
    }
  }
  
  while (i<rts->length) { 
    sts->x[k]    = rts->x[i];
    sts->y[k]    = rts->y[i];
    ouh1->sw->pdf[k] =inh1->sw->pdf[i]; 
    ouh2->sw->pdf[k] = 0;
    i++;
    k++;
  }
  while (j<wts->length) { 
    sts->x[k]    = wts->x[j];
    sts->y[k]    = wts->y[j];
    ouh1->sw->pdf[k] = 0;
    ouh2->sw->pdf[k] = inh2->sw->pdf[j]; 
    j++;
    k++;
  }
  sts->length = k;
  DFFromPDF(swo1->df,swo1->pdf,swo1->sortedts->length,1);
  DFFromPDF(swo2->df,swo2->pdf,swo2->sortedts->length,1);
  
  
}
*/

int verify(Mat *t,int K,int length) { 
  int i;
  int e=1;

  for (i=1;i<length;i++) { 
    if ((K && t[i]>t[i-1]) || (!K &&t[i]<t[i-1])) { 
      printf(" %d ",i);
      e=0;
    }
   }
  printf("\n");

  return e;
}


// assuming the histograms have been sorted
void 
mergeExtendHistogramNumericLabel(HistogramNumericLabel *inh1, HistogramNumericLabel *inh2,
				 HistogramNumericLabel *ouh1, HistogramNumericLabel *ouh2,
				 int asc) {
  

  int N = inh1->length+inh2->length;
  int i,j,k; 

  
  ouh1->word   = (Mat*) calloc(N,sizeof(Mat));
  ouh1->number = (Mat*) calloc(N,sizeof(Mat));
  ouh2->word   = (Mat*) calloc(N,sizeof(Mat)); 
  ouh2->number = (Mat*) calloc(N,sizeof(Mat));
  assert(ouh1->word);

  i = j = k=0;

  while (i<inh1->length && j<inh2->length) { 
 
    
    if ( 
	(asc && (LTX(inh1->word[i],inh2->word[j])))
	|| 
	(!asc && (GTX(inh1->word[i],inh2->word[j])) )
	)

      {
	ouh1->word[k]    =inh1->word[i]; 
	ouh1->number[k]  =inh1->number[i]; 
	ouh2->word[k]   =inh1->word[i]; 
	ouh2->number[k]  = 0;
	i++;
      }
    else 
      if (
	  ( asc && (GTX(inh1->word[i],inh2->word[j])))	  
	  ||
	  (!asc && (LTX(inh1->word[i],inh2->word[j]))) 

	  )
	{ 
	  ouh1->word[k]    =inh2->word[j]; 
	  ouh1->number[k]  = 0;
	  ouh2->word[k]   = inh2->word[j]; 
	  ouh2->number[k]  = inh2->number[j]; 
	  j++;
	}
      else 
	{ 
	  ouh1->word[k]    = inh1->word[i]; 
	  ouh1->number[k]  = inh1->number[i]; 
	  ouh2->word[k]   = inh2->word[j]; 
	  ouh2->number[k]  = inh2->number[j]; 
	  j++;
	  i++;
	}
    k++;
 
  }

  while (i<inh1->length) { 
      ouh1->word[k]    =inh1->word[i]; 
      ouh1->number[k]  =inh1->number[i]; 
      ouh2->word[k]   =inh1->word[i]; 
      ouh2->number[k]  = 0;
      i++;
      k++;
  }
  while (j<inh2->length) { 
    ouh1->word[k]    =inh2->word[j]; 
    ouh1->number[k]  = 0;
    ouh2->word[k]   = inh2->word[j]; 
    ouh2->number[k]  = inh2->number[j]; 
    j++;
    k++;
  }
  
  ouh1->length = k;
  ouh2->length = k;
  
}
