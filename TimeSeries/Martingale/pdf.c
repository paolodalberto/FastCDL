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
#define MARTINGALEPDF_MODULE


#include<pdf.h>

static int debug = 0;


static
int build_adj(Adj *s, DistanceFunctions dist) { 
  
  int i,j;
  double *a = s->adj;
  int N   =s->max;
  int l   = s->length;
  double min;
  
  //s->length = s->ts->length;


  for (i=0;i<l-1;i++) {
    int count =0;
    min = a[N*i+i+1] = dist(s->ts->array[i].y,s->ts->array[i+1].y,s->ts->array[i].dimensions);
    for (j=i+2;j<l;j++) { 
      a[N*i+j] = dist(s->ts->array[i].y,s->ts->array[j].y,s->ts->array[i].dimensions);
      if (!a[N*i+j]) count ++;
      if ( min >a[N*i+j] && a[N*i+j]) { 
	min = a[N*i+j];
	s->mins[i] = j;
      }
    }
    for (j=i;j<i;j++) { 
      a[N*i+j] = a[N*j+i];
      if (!a[N*i+j]) count ++;
      if ( min >a[N*i+j] && a[N*i+j]) { 
	min = a[N*i+j];
	s->mins[i] = j;
      }
    }
    s->pdf[i] = (1.0 + count)/ a[N*i+s->mins[i]];

  }
  
  return s->length-1;



}




#define NEXT(I,L) (((I)+1)%(L))
#define LAST(beginning,L) ((beginning)+(L))%(L)


static 
int update_adj(Adj *s, 
	       SinglePoint *t, 
	       DistanceFunctions dist) { 
  

  int i,j,count;
  double *a = s->adj;
  int N   = s->max;
  int l   = (s->length>=s->max)?s->max:(++(s->length));
  double min;
  int last;
  
  
  // introduce the element in the circular buffer 
  SHIFTADD(s->ts,*t);
  
  
  if (l==1) { // I need at least two points to apply any distance measure
    s->top = NEXT(s->top,N);
    s->mins[0] = 1;
    return 0;
  }


  count=1;
  // update the column
  min = 0;
  for (i=0;i<l-1;i++) { 
    // the matrix is symmetric 
    a[N*(s->top) +i] = a[N*i+s->top] = dist(s->ts->array[i].y,s->ts->array[s->top].y,s->ts->array[i].dimensions);
    
    if (!a[N*i+s->top])  // when i == s->top this will be 0, at least once this will happen
      count ++;
    else 
      if ( !min || min > a[N*i+s->top]) {
	min = a[N*i+s->top];
	s->mins[s->top] = i; // an arbitrary minimum different from itself or duplicate
      }
    
  }

  // update the pdf : that is the minimum for each row/column 
  for (i=0;i<l;i++) {
    min = a[N*i+s->mins[i]]; // current minimum edge in this row

    if (min> a[N*i+s->top] &&  a[N*i+s->top]) {   // if the new point is the  closest
      s->mins[i] = s->top;
    }
    else 
      if (s->mins[i]==s->top) { /* the current point is not the closest but the previous point was 
				  then I have to find a new edge 
			       */
	for (j=0;j<l;j++) { // search the row  
	  if (min> a[N*i+j] && a[N*i+j]) { 
	    s->mins[i] = j;
	    min = a[N*i+j];
	  }
	  
	}
      
      }
      else { /* nothing to do, the previous minimum is just fine */ } 

    if (a[N*i+s->mins[i]]) 
      s->pdf[i] = (count)/ a[N*i+s->mins[i]];
    else 
      s->pdf[i] =  0;
  }
  
  if (debug) { 
    PRINTSP(*t);
    PRINTF("\nPDF");
    for (i=0;i<l;i++)
      PRINTF("%e ",s->pdf[i]);
    PRINTF("\nMINS");
    for (i=0;i<l;i++)
      PRINTF("%d ",s->mins[i]);
    PRINTF("\nADj");
    for (i=0;i<l;i++) {
      for (j=0;j<l;j++) 
	PRINTF("%e ",s->adj[N*i+j]);
      PRINTF("\n");
    }

  }

  last = s->top;
  s->top = NEXT(s->top,N);

  return last;
}

static 
int update_adj_average(Adj *s, 
		       SinglePoint *t, 
		       DistanceFunctions dist) { 
  

  int i,j,count;
  double *a = s->adj;
  int N   = s->max;
  int l   = (s->length>=s->max)?s->max:(++(s->length));
  double min;
  int last;
  
  
  // introduce the element in the circular buffer 
  SHIFTADD(s->ts,*t);
  
  
  if (l==1) { // I need at least two points to apply any distance measure
    s->top = NEXT(s->top,N);
    return 0;
  }


  count=1;
  // update the column
  min = 0;
  

  
  

  for (i=0;i<l;i++) { 

    s->pdf[i] -= a[N*i+s->top]/(N-1);
    // the matrix is symmetric 
    a[N*(s->top) +i] = a[N*i+s->top] = dist(s->ts->array[i].y,s->ts->array[s->top].y,s->ts->array[i].dimensions);
    
    if (!a[N*i+s->top])  // when i == s->top this will be 0, at least once this will happen
      count ++;

    s->pdf[i] += a[N*i+s->top]/(N-1);

  }

  s->pdf[s->top] = 0;
  for (i=0;i<l;i++) {
    if (i!=s->top)
      s->pdf[s->top] += a[N*(s->top)+i]/(N-1);
  }

  
  if (debug) { 
    PRINTSP(*t);
    PRINTF("\nPDF");
    for (i=0;i<l;i++)
      PRINTF("%e ",s->pdf[i]);
    PRINTF("\nMINS");
    for (i=0;i<l;i++)
      PRINTF("%d ",s->mins[i]);
    PRINTF("\nADj");
    for (i=0;i<l;i++) {
      for (j=0;j<l;j++) 
	PRINTF("%e ",s->adj[N*i+j]);
      PRINTF("\n");
    }

  }

  last = s->top;
  s->top = NEXT(s->top,N);

  return last;
}


// return: the adjacent matrix from the time series ts
// as side effect: the time series receive the pdf value for comparison purpose later on
// input: N is the size of the Adj or time series 
// input: adjacent matrix s 
  
Adj* pdf_based_on_distance(TimeSeries *ts, unsigned int N, Adj *s) {
  int i;
  int j;
  

  if (!s) { 

    ALLOCATE_ADJ(s,N,ts->dimensions);
    /*
      s->length = ts-> length;
      for (i=0;i<ts->length;i++) { 
      COPYSP(s->ts->array[i],ts->data[i]);
      }
      build_adj(s,Euclide);
      for (i=0;i<ts->length;i++) { 
      ts->data[i].pdf = s->pdf[i];
      }
    */
  }
  
  for (i=0;i<ts->length;i++) { 
#ifdef   MINIMUM_DISTANCE   
    j = update_adj(s,ts->data + i, Euclide);
#endif
#ifdef   AVERAGE_DISTANCE   
    j = update_adj_average(s,ts->data + i, Euclide);
#endif
    
    ts->data[i].pdf  = s->pdf[j];
  }
    

    
    
  
  return s;
}

// sorted decreasing decreasing 
int binary_search_array(double *array, double  sp, int stat_len) { 
  int found = stat_len==0;
  int i   = stat_len/2;
  int len = stat_len;
  int left = 0;
  double pdf = sp;

  
  while (!found) { 

    if (debug)  { 
      PRINTF(" length=%d and i = %d pdf = %e\n", len,i,array[i]);  
      
    }
   
    if (len ==0) 
      break;
    
    if (pdf==array[i] || (i>0 &&  pdf< array[i-1] && pdf>array[i])  )
      found = 1;
    else {
      if (pdf>array[i]) {
	len = len/2;
	i = left + len/2;
	
      } else { 
	len =  left+len-1-i;
	left = i+1;
	//	len = (len%2)?(len/2+1):(len/2);
	i = left + len/2;
	
      } 
    }
    if ( (len == 0) || i >= stat_len) { 
      i=-1;
      found =1;
    }
  } 

  if (found) return i;
  else return -1;
  
}



//sp should be already in the time series and the time series is already in the adjacent matrix 

double pdfbased_distance_pvalue(TimeSeries *ts, SinglePoint *sp, void *state) {

  double prob=-1;
  TimeSeries *t;
  int k;
  Window *w;
  MartingaleState *ms = (MartingaleState *) state;


  ALLOCATETS(t,1,sp->dimensions);
  ASSIGN_SINGLEPOINT(t,0,sp);
  t->length = 1;


  w = ((MartingaleState *)state)->w;
  if (debug) { 
    PRINTWINDOW(w,0);
  }


  w = WindowUpdate(w,t,((MartingaleState *)state)->max);
  ((MartingaleState *)state)->w = w;
  if (debug) { 
    PRINTWINDOW(w,0);
  }


 

  ms->a = pdf_based_on_distance(t,ms->max,ms->a);
  sp->pdf = t->data[0].pdf;

  if (ms->a->length>=ms->max) { 
    memcpy(w->sw->pdf,ms->a->pdf,sizeof(double)*ms->a->length);
    SORT_ARRAY(w->sw->pdf,ms->a->length);
    
    if (debug) { 
      int i;

      PRINTF("PDF:");
      for (i=0;i< ms->a->length;i++) 
	PRINTF("%e ",ms->a->pdf[i]);
      PRINTF("Sorted\n");
      PRINTF("Sorted PDF:");
      for (i=0;i< ms->a->length;i++) 
	PRINTF("%e ",w->sw->pdf[i]);
      PRINTF("Sorted\n");

    }
    
    k = binary_search_array(w->sw->pdf,sp->pdf,ms->a->length);
    if (debug) { 
      PRINTF("RANK %d of value %e\n",k, sp->pdf);
      
    }
    prob = 0;
    if (k>=0) { 
#ifdef   MINIMUM_DISTANCE   
      prob = ms->a->length - k;
#endif
#ifdef   AVERAGE_DISTANCE   
      prob = k;
#endif
      prob = prob / ms->a->length;
    }
  }
  
  if (debug) { 
    PRINTF("v %e x %ld \n",prob,sp->x); 
    
  }
  FREETS(t);
  return prob;

}
