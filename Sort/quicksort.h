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
#ifndef __QUICK_SORT_H__
#define __QUICK_SORT_H__

#include <stdlib.h>
#include <interface.h>
#include <window.h>
#define DOUBLE_PRECISION 1


#if(SINGLE_PRECISION)
#define ABS(x) fabsf((x))
#endif
#if(DOUBLE_PRECISION)
#define ABS(x) fabs((x))
#endif
#if(SINGLE_COMPLEX || DOUBLE_COMPLEX)
#define ABS(x) cabs((x))
#endif







#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  
  // Histograms

  extern void UnionIntersectionHistogram(Histogram *inh1, Histogram *inh2,
					 Histogram *un, Histogram *in) ;
  
  extern void  extendHistogram  (Histogram *inh1, Histogram *inh2, Histogram *ouh1, Histogram *ouh2);
  extern void  insort_histogram (register Histogram *array, register int lower, register int upper);
  extern void  sorthistogram    (register Histogram *array, register int lower, register int upper);


  extern void mergeExtendHistogramNumericLabel(HistogramNumericLabel *inh1, HistogramNumericLabel *inh2,
					       HistogramNumericLabel *ouh1, HistogramNumericLabel *ouh2,
					       int asc);


  
#define SORT_HISTOGRAM(h) sorthistogram(h,0,h->length-1)

  // Time Series 
  
  extern void  extendWindow  (Window *inh1, Window *inh2, Window *ouh1, Window *ouh2, int key);
  extern void  insortTimeSeries(register TimeSeries *array, register int lower, register int upper,int key);
  extern void  sortTimeSeries(register TimeSeries *array, register int lower, register int upper,int key);
  
#define SORT_TIMESERIES(ts,x) { sortTimeSeries(ts,0,ts->length-1,x); if (debug) verifyT(ts,x); }


  // basic Sort routines for arrays

#define SORT_ARRAY(a,len) partial_quickersort (a,0, len-1);
#define SORT(x,y)  partial_quickersort((x),0,((y)-1))
#define SORTI(x,y) partial_quickersort_inverse((x),0,((y)-1))
  
  extern void  partial_quickersort (register Mat *array, register int lower, register int upper);
  extern void  partial_quickersort_inverse (register Mat *array, register int lower, register int upper);
  extern void  insort (register Mat * array, register int len);

  


  
  // Basic comparison routines

  extern int verifyT(TimeSeries * array, int key);
  extern int verify(Mat *t,int K, int length);
  extern void bottom(SinglePoint *m, SinglePoint *tem);
  extern void top(SinglePoint *m, SinglePoint *tem);
  extern int GT(SinglePoint x, SinglePoint y);
  extern int LT(SinglePoint x, SinglePoint y);
  extern int GE(SinglePoint x, SinglePoint y);
  extern int LE(SinglePoint x, SinglePoint y);
  extern int EQ(SinglePoint x, SinglePoint y);
  extern int NE(SinglePoint x, SinglePoint y);




  
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
