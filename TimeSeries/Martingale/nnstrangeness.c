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
#define  STRANGENESS_MODULE
#include<nnstrangeness.h>

static int debug =0 ;

#ifdef RDEF 
#include <R.h>
#define PRINTF Rprintf
#else 
#define PRINTF printf
#endif


double *min_cluster_alpha_i(TimeSeries *ts) { 
  int i,j;
  double *alpha_i = (double *) calloc(ts->length,sizeof(double));
  for (i=0;i<ts->length-1;i++) {
    alpha_i[i] = euclidean_distance(ts->data[i],ts->data[i+1]);
  }
  alpha_i[i]  = euclidean_distance(ts->data[i],ts->data[0]);
  //  fabs(ts->y[i]- ts->y[0]);

  for (i=0;i<ts->length-1;i++) {
    for (j=i+1;j<ts->length;j++) {
      double t = euclidean_distance(ts->data[i],ts->data[j]);
      if ( t< alpha_i[i])
	alpha_i[i] = t;
    
    }
  }

  return alpha_i;


}



TimeSeries *update_time(TimeSeries *ts, SinglePoint *sp) { 

  TimeSeries *t;

  if (ts) { 
    ALLOCATETS(t,ts->length + 1,ts->dimensions);
    COPY_TS(t, ts)
  } else { 
    ALLOCATETS(t,1,sp->dimensions);
  } 
  
  ASSIGN_SINGLEPOINT(t,((t->max)-1),sp)
  t->length= t->max;  
  if (t->length>1) SORT_TIMESERIES(t,TIMESLOT);

  return t;
}


int binary_search_time_sorted_series(TimeSeries *ts, SinglePoint *sp) { 
  int found = 0;
  int i=ts->length/2;
  int len = ts->length;
  int left = 0;
  if (debug) { 
    PRINTF(" Binary Search in the time series : ");
    PRINTTIMESERIES(ts,0);
    PRINTSP(*sp);PRINTF(" <----\n ");
  
  }
    

  while (!found) { 

    if (debug)  PRINTF(" length=%d and i = %d value %ld vs %ld \n", len,i,ts->data[i].x,sp->x);
   
    if (len ==0) 
      break;
    
    if (sp->x==ts->data[i].x || (i>0 &&  sp->x<ts->data[i-1].x && sp->x>ts->data[i].x)  )
      found = 1;
    else {
      if (sp->x>ts->data[i].x) {
	len = len/2;
	i = left + len/2;
	
      } else {
	len =  left+len-1-i;
	left = i+1;
	//len = (len%2)?(len/2+1):(len/2);
	i = left + len/2;
	
      } 
    }
    if ( (len == 0) || i >= ts->length) { 
      i=-1;
      found =1;
    }
  } 

  if (found) return i;
  else return -1;
  
}




static inline 
double min_cluster_distance_strangeness_pvalue(TimeSeries *ts, SinglePoint *sp, void *state, Alpha_i A) { 

  double *alpha_i; 
  int p_n;
  int i,j; 
  Window *w;

  
  
  TimeSeries *temp =0;

  //UPDATETS(temp,ts,sp);
  temp = update_time(ts,sp);


  
  w = ((MartingaleState *)state)->w;
  w = WindowUpdate(w,temp,((MartingaleState *)state)->max);
  ((MartingaleState *)state)->w = w;

  FREETS(temp);			      

  
  if (w->ts->length< ((MartingaleState *)state)->max) 
    return -1;



  

  j =  binary_search_time_sorted_series(w->ts,sp);
  alpha_i =  A(w->ts);

  if (debug) { 
    for (i=0;i<w->ts->length;i++) {
      PRINTF("alpha_i[%d] %e \n",i,alpha_i[i]);
    }
  }


  
  p_n = 0;
  for (i=0;i<w->ts->length;i++) { 
    if (i!=j && alpha_i[i]>= alpha_i[j]) 
      p_n ++;
  }

  free(alpha_i);
  
  return (double)p_n/(double) w->ts->length;


}



double min_cluster_distance_pvalue(TimeSeries *ts, SinglePoint *sp, void *state) { 

  return min_cluster_distance_strangeness_pvalue(ts,sp,state, min_cluster_alpha_i);

}





