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
#define WINDOW_MODULE 1

#include <type.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <quicksort.h>

#include <window.h>
#include <doubly_compensated_sumc.h>

int static debug =0;




Window *allocatew(unsigned int N, unsigned int k) { 
    Window *w= (Window *) calloc(1,sizeof(Window));			       
    assert(w);								
    strcpy((w)->comment,NONCOMPACT);					
    ALLOCATETS((w)->ts,N,k);						
    {						
      StatisticalWindow *sw = (StatisticalWindow *) calloc(1,sizeof(StatisticalWindow));	
      assert(sw);								
      ALLOCATETS((sw)->sortedts,N,k);					
      (sw)->pdf =(double*) calloc(4*N,sizeof(double));			
      assert((sw)->pdf);							
      (sw)->df  = (sw)->pdf+N;						
      (sw)->ext_pdf  = (sw)->pdf+2*N;					
      (sw)->ext_df  = (sw)->pdf+3*N;
      w->sw = sw;
    }/*ALLOCATESW((w)->sw,N,k);						 */
    /*ALLOCATE_POSET((w)->poset,N);						*/

    return w;
}

double euclidean_distance(SinglePoint r, SinglePoint l) {
  

  return Euclide(r.y, l.y,r.dimensions);
  /*
  int k;
  double t,d;

  d =0;
  
  for (k=0;k<r.dimensions;k++) {    
    t = r.y[k] -  l.y[k];    
    t = t*t;
    d += t;
  }    

  return sqrt(d);
  */
}


SinglePoint SP_add(SinglePoint *res, SinglePoint l, SinglePoint r) { 
  
  SP_ADD(*res, = ,l,+,r);
  return *res;
} 

#ifndef MIN 
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

SinglePoint SP_min(SinglePoint *res, SinglePoint l, SinglePoint r) { 
  
  double t,d;
  int k;
  
  for (k=0;k<r.dimensions;k++) {    
    (*res).y[k] = MIN(r.y[k],l.y[k]);    
  }    
  
  return *res;
} 

SinglePoint SP_abs(SinglePoint *res, SinglePoint l) { 
  
  int k;
  
  for (k=0;k<l.dimensions;k++) {    
    res->y[k] = fabs(l.y[k]);
  }    

  return *res;
} 


SinglePoint SP_sub(SinglePoint *res, SinglePoint l, SinglePoint r) { 
  
  SP_ADD(*res, = ,l,-,r);
  return *res;
} 


SinglePoint SP_mul(SinglePoint *res, Mat l, SinglePoint r) { 
  
  SP_MUL(*res, = ,l,*,r);
  return *res;
} 





char * 
PrintSP(SinglePoint *t, int identation, int P) { 
  char *temp=0;

  if (t) { 
    int k;
    char *line = (char *) calloc((64*t->dimensions+identation+128),sizeof(char));
    temp       = (char *) calloc((t->dimensions*3*64+3*identation+256),sizeof(char));

    SPRINT(temp,line,"[ C %u ", t->code);
    SPRINT(temp,line,"D %u ", t->dimensions);          

    SPRINT(temp,line,"X %ld,  (", t->x);          
    for (k=0;k<t->dimensions;k++) { 

      //if (debug) {PRINTF("%d \n",k); }
      SPRINT(temp,line,"%e ",t->y[k]);           
    }                                    
    SPRINTNA(temp,line,") ]");                         
    free(line);
    if (P==1) { 
      PRINTF("%s",temp);
    }
  }
  return temp;
}

SinglePoint * 
readSP(int identation, char **temp) { 
  SinglePoint *t = 0;
  char *running=*temp;
  unsigned int sizeinbytes;
  unsigned int max;
  unsigned int length;
  unsigned int dimensions;
  unsigned int code;
  if (running) {
    int n,i,j;
    if (debug) { 
      PRINTF("READ SP %s \n",running);
    }
    running += identation;
    sscanf(running,"[ C %u D %u X %u,  (",&code,&dimensions,&max);
    
    FINDCHAR(running,'(',n);
    running += n+1; 
    
    ALLOCATE_SP(t,dimensions);
    t->x=max;
    t->code = code;

    FINDCHAR(running,' ',n);
    running[n] = 0;
    if (debug) { 
      PRINTF("dimensions %d ",dimensions);
      PRINTF("%s \n",running);
    }
    for (j=0;j<dimensions;j++) { 
      t->y[j] = atof(running);
      FINDCHAR(running,' ',n);
      running[n] = 0;
      running += n +1;
    
    }

    FINDCHAR(running,']',n);
    running += n+1; 
  }

  *temp = running;

  // deallocation of the string is not my problem 
  return t;
 

}

/**********************
 * These can be used for debug but also to store data in a file and then read it.
 *
 */ 
char * 
PrintTimeseries(TimeSeries *t, int identation, int P) { 
  int i,j;
  char *temp=0;
  if (t) { 
    char *line = (char *) calloc((/*t->length*/ t->max*64*t->dimensions+identation+128),sizeof(char));
    temp       = (char *) calloc((t->dimensions*8*/*t->length*/ t->max*64+3*identation+256),sizeof(char));
    
    IDENTS(identation,temp,line); 
    sprintf(line,"TS B %u Max %u L %u D %u\n",t->sizeinbytes,t->max, t->length,t->dimensions);
    strcat(temp,line);
    IDENTS(identation,temp,line); SPRINTNA(temp,line,"T     : ");
    if (t->data) {
      
      for (i=0;i</*t->length*/ t->max;i++) { 
	char *sp = PrintSP(&t->data[i],0,0);
	if (debug) {PRINTF("---- SP %s \n",sp);}
	strcat(temp,sp);
	free(sp);
      }
      
      SPRINTNA(temp,line,"\n");
    }
    free(line);
    if (P==1) PRINTF("%s\n",temp);

  }

  // deallocation of the string is not my problem 
  return temp;
 
}

char* 
PrintStatisticalWindow(StatisticalWindow *t, int identation, int P) { 
  int i;
  char *temp=0;
  if ( t) { 
 
    int len = t->sortedts->max*64*t->sortedts->dimensions+identation+128;

    char *line = (char *) calloc((len),sizeof(char));
    temp       = (char *) calloc((8*len),sizeof(char));

    
    IDENTS(identation,temp,line); 
    sprintf(line,"SW  B %u %d \n",t->sizeinbytes,
	    (t->sortedts)?t->sortedts->max:0);
    strcat(temp,line);
    IDENTS(identation,temp,line); SPRINTNA(temp,line,"Sorted List \n");
    if (t->sortedts) { 
     char * ts = PrintTimeseries(t->sortedts,identation+3,0);
     if (ts)  {
       strcat(temp,ts);
       free(ts); // because I need to deallocate the temporary string 
     }
    }
    IDENTS(identation,temp,line); 
    sprintf(line,"stat line %d \n",t->statistics_len);
    IDENTS(identation,temp,line); SPRINTNA(temp,line,"PDF: ");
    if (t->pdf) 
      for (i=0;i<t->sortedts->max;i++) { 
	SPRINT(temp,line,"%e ",t->pdf[i]);
      }
    SPRINTNA(temp,line,"\n");
    IDENTS(identation,temp,line); SPRINTNA(temp,line,"Ext_PDF: ");
    if (t->ext_pdf) 
      for (i=0;i<t->sortedts->max;i++) { 
	SPRINT(temp,line,"%e ",t->ext_pdf[i]);
      }
    SPRINTNA(temp,line,"\n");
    IDENTS(identation,temp,line); SPRINTNA(temp,line,"DF : ");
    if (t->df)
      for (i=0;i<t->sortedts->max;i++) { 
	SPRINT(temp,line,"%e ",t->df[i]);
      }
    SPRINTNA(temp,line,"\n");
    IDENTS(identation,temp,line); SPRINTNA(temp,line,"Ext_DF : ");
    if (t->ext_df)
      for (i=0;i<t->sortedts->max;i++) { 
	SPRINT(temp,line,"%e ",t->ext_df[i]);
      }
 
    SPRINTNA(temp,line,"\n");
    free(line);
   
    if (P==1) PRINTF("%s\n",temp);
  }

  // deallocation of the string is not my problem 
  return temp;
}

char * 
PrintWindow(Window *t, int identation, int P) { 
  char *temp=0;
 
  if (t) { 
    
    int len = t->ts->max*64*t->ts->dimensions+identation+128;

    char *line = (char *) calloc((len),sizeof(char));
    temp       = (char *) calloc((16*len),sizeof(char));
 
    IDENTS(identation,temp,line);
    sprintf(line," W %u Comment %s, %d \n",t->sizeinbytes,t->comment,(t->ts)?t->ts->max:0);
    
    strcat(temp,line);
    if (t->ts) { 
      char * ts = PrintTimeseries(t->ts,identation+3,0);
      if (ts) {
	strcat(temp,ts);
	free(ts); // because I need to deallocate the temporary string 
      }
    }
    
    if (t->sw) { 
      char * ts =PrintStatisticalWindow(t->sw,identation+3,0);
      if (ts) { 
	strcat(temp,ts);
	free(ts); // because I need to deallocate the temporary string 
      }
    }

    free(line);
    if (P==1) PRINTF("%s\n",temp);

  }
  // deallocation of the string is not my problem 
  return temp;

}
char *
PrintCircular(CircularBuffer *t, int identation,int P) { 
  int i,j;
  char *temp=0;

  char *line = (char *) calloc((t->length*64*t->array[0].dimensions+identation+128),sizeof(char));
  temp       = (char *) calloc((2*t->length*64*t->array[0].dimensions+3*identation+128),sizeof(char));

 
  IDENTS(identation,temp,line);
  sprintf(line,"C B %u top %d length %d D %u\n",t->sizeinbytes,t->top,t->length,t->array[0].dimensions);
  strcat(temp,line);
  
  IDENTS(identation,temp,line);SPRINTNA(temp,line,"Array: ");
  if (t->array) 
    j=0;
    for (i=t->top;
	  j< (t->length);
	 (i++)%t->length,j++) {
      char *sp = PrintSP(&(t->array[i]),0,0);
      strcat(temp,sp);
      free(sp);
    }
  SPRINTNA(temp,line,"\n");

  free(line);
  if (P==1) PRINTF("%s\n",temp);

  // deallocation of the string is not my problem 
  return temp;

  
}


/********************************** 
* REading from string 
*
*
*/





TimeSeries * 
ReadTimeseries(int identation, char **temp) { 
  TimeSeries *t = 0;
  char *running=*temp;
  unsigned int sizeinbytes;
  unsigned int max;
  unsigned int length;
  unsigned int dimensions;

  if (running) {
    int n,i,j;
    // alocate space for the time series node
    //    t = (TimeSeries *) calloc(1,sizeof(TimeSeries));
    //assert(t);

    
    running += identation;
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"TS B %u Max %u L %u D %u",&sizeinbytes,&max, 
	   &length, &dimensions);
    running = running + n +1;
    FINDCHAR(running,':',n);
    running += n+2; 
    
    ALLOCATETS(t,max,dimensions);
    t->length = length;
    for (i=0;i<t->max;i++) { 
      SinglePoint *sp = readSP(0,&running);
      COPYSP(t->data[i],*sp); FREE_SP(sp);
    }
    FINDSLASHN(running,n);
    running[n] = 0;
    running += n+1;
  }

  *temp = running;

  // deallocation of the string is not my problem 
  return t;
 
}

StatisticalWindow *
ReadStatisticalWindow(int identation, char **temp) { 
  StatisticalWindow *t=0;
  char *running = *temp;

  if ( running) { 
    int n,i,data;
    int r;

    t = (StatisticalWindow*) calloc(1,sizeof(StatisticalWindow));
    assert(t);
    
    running +=identation;
    FINDSLASHN(running,n);
    running[n] = 0;
    r = sscanf(running,"SW  B %u %d ",&(t->sizeinbytes),&data);
    running += n +1;

    // sorted list line is off
    FINDSLASHN(running,n);
    running += n +1;
    

    if (data) {
      t->sortedts = ReadTimeseries(identation+3,&running);

      
      FINDSLASHN(running,n);
      running[n] = 0;
      sscanf(running,"stat line %d \n",&t->statistics_len);
      running += n +1;
      
      FINDCHAR(running,':',n);
      running += n+2; 
      
      
      t->pdf = (double *) calloc(2*t->sortedts->max,sizeof(double));
      assert(t->pdf);
      for (i=0;i<t->statistics_len;i++) { 
	FINDCHAR(running,' ',n);
	running[n] = 0;
	t->pdf[i] = atof(running);
	running += n +1;

      }
      FINDSLASHN(running,n);
      running[n] = 0;
      t->pdf[i] = atof(running);
      running += n +1;
    
      FINDCHAR(running,':',n);
      running += n+2; 

      
      t->df = t->pdf+(t->sortedts->max);
      for (i=0;i<t->statistics_len;i++) { 
	FINDCHAR(running,' ',n);
	running[n] = 0;
	t->df[i] = atof(running);
	running += n +1;
	
      }
      FINDSLASHN(running,n);
      running[n] = 0;
      t->df[i] = atof(running);
      running += n +1;
    }
  }

  *temp = running;
  // deallocation of the string is not my problem 
  return t;
}

Window * 
ReadWindow(int identation, char **temp) { 
  char *running=*temp;
  Window *t=0;
  if (running) { 
    int n,data,r;
    
    t = (Window *) calloc(1, sizeof(Window));
    assert(t);
    running += identation;
    FINDCHAR(running,',',n);
    running[n] = 0;
    
    r = sscanf(running," W %u Comment %s",&(t->sizeinbytes),t->comment);
    running += n +1;
    FINDSLASHN(running,n);
    running[n] = 0;
    r = sscanf(running," %d ",&data);
    running += n +1;


    if (data) { 
      t->ts = ReadTimeseries(identation+3,&running);
      t->sw = ReadStatisticalWindow(identation+3,&running);
    }

  }
  // deallocation of the string is not my problem 

  *temp = running;
  return t;

}
CircularBuffer *
ReadCircular(int identation,char **temp) { 

  char *running=*temp;
  unsigned int sizeinbytes;
  int top;
  unsigned int length;
  unsigned int dimensions;
  int j;
  CircularBuffer *t;// = (CircularBuffer *) calloc(1,sizeof(CircularBuffer));


  if (running) {
    int n,i;
    running +=identation;
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"C B %u top %d length %u D %u",&sizeinbytes,&top,&length,&dimensions);

    ALLOCATECIRCULAR(t,length,dimensions);

    running += n +1;

    
    FINDCHAR(running,':',n);
    running += n+2; 
    if (t->length) {
      
      for (i=0; i< t->length; i++) {
	SinglePoint *sp = readSP(0,&running);
	COPYSP(t->array[i],*sp);
	FREE_SP(sp);
      }
      
      FINDSLASHN(running,n);
      running[n] = 0;
    }
    
    *temp = running;
  }
  // deallocation of the string is not my problem 
  return t;

  
}







/********************************** 

*

*/

void copy_ts(TimeSeries *r, TimeSeries *ts) {                                                  
  int _i;                
  if (debug) { 
    PRINTF("Copy ts \n");
    PRINTTIMESERIES(r,0); 
    PRINTTIMESERIES(ts,0); 
  }
  for (_i=0;_i<ts->length;_i++) {
    
    if (debug) { PRINTSP(r->data[_i]);PRINTSP(ts->data[_i]); PRINTF("\n"); }
    
    r->data[_i].x = ts->data[_i].x;                                      
    memcpy((r)->data[_i].y,
	   ts->data[_i].y, 
	   (ts)->dimensions*sizeof(Mat)); 


    /* ASSIGN_SINGLEPOINT(r,_i,ts->data +_i);  */
    if (debug) { PRINTSP(r->data[_i]); PRINTF("\n"); }
  } 
  r->length=ts->length;                                                           
  if (debug) { 
    PRINTTIMESERIES(r,0); 
    PRINTTIMESERIES(ts,0); 
    PRINTF("Copy ts <-\n");
  }
 

}


void copy_ts_b(TimeSeries *r, TimeSeries *ts, int from, int to) {                                                  
  int _i;    
  int j=0;            
  if (debug) { 
    PRINTF("Copy ts \n");
    PRINTTIMESERIES(r,0); 
    PRINTTIMESERIES(ts,0); 
  }

  for (_i=from;_i<ts->length && _i<to;_i++,j++) {
    
    if (debug) { PRINTSP(r->data[_i]);PRINTSP(ts->data[_i]); PRINTF("\n"); }
    
    r->data[j].x = ts->data[_i].x;                                      
    memcpy((r)->data[j].y,
	   ts->data[_i].y, 
	   (ts)->dimensions*sizeof(Mat)); 


    /* ASSIGN_SINGLEPOINT(r,_i,ts->data +_i);  */
    if (debug) { PRINTSP(r->data[_i]); PRINTF("\n"); }
  } 
  r->length=to-from;                                                           
  if (debug) { 
    PRINTTIMESERIES(r,0); 
    PRINTTIMESERIES(ts,0); 
    PRINTF("Copy ts <-\n");
  }
 

}


Window *WindowFromTimeSeries(TimeSeries *ts) { 

  int N = ts->length;
  Window *r;
  int i;
  if (debug) { PRINTTIMESERIES(ts,0); } 
  ALLOCATEW(r,N,ts->dimensions);
  if (debug) { PRINTWINDOW(r,0); }
  copy_ts(r->ts,ts);
  if (ts->length>1) SORT_TIMESERIES(r->ts,TIMESLOT);


  return r;
}

Window *WindowFromTimeSeriesMax(TimeSeries *ts, int MAX) { 

  int N = (MAX>ts->length)?ts->length:MAX;
  //int M = (MAX<ts->length)?ts->length:MAX;
  Window *r;
  int i;

  ts->length = N;
  ALLOCATEW(r,N,ts->dimensions);
  
  COPY_TS(r->ts,ts);
  if (ts->length>1) SORT_TIMESERIES(r->ts,TIMESLOT);

  return r;
}

#ifndef MAX 
#define MAX(a,b) (((a)<(b))?(b):(a))
#endif





void append_ts(TimeSeries *r, TimeSeries *ts) { 
  int _i;                       
  
  if (debug) { PRINTF("append ts\n"); PRINTTIMESERIES(r,0); }  

  for (_i=0; _i< (ts)->length; _i++)  {                  
    if (debug) { 
      PRINTSP(r->data[_i+r->length]);
      PRINTF("\n");
    }
    
    ASSIGN_SINGLEPOINT((r),_i+r->length,(ts)->data +_i);  
    /*
      (r)->data[_i+r->length].x = (ts)->data[_i].x;                                      
      memcpy((r)->data[_i+r->length].y,(ts)->data[_i].y, (ts)->dimensions*sizeof(Mat)); 
    */
    if (debug) { 
      PRINTSP(r->data[_i+r->length]);
      PRINTF("\n");
    }
	

  }                                                    
  r->length+=ts->length;
  
  if (debug) { PRINTTIMESERIES(r,0); }  

}


Window *WindowUpdate(Window *r, TimeSeries *ts, int max) { 

  TimeSeries *s,*g;
  Window *res;

  if (!r)  { 
    return WindowFromTimeSeriesMax(ts,max);
  } else {
    int m;
     
    m = MAX(r->ts->max, ts->length+r->ts->length);
    m = MAX(max,m);
    if (debug) { PRINTF("update %d %d %d \n",ts->length,r->ts->length,m); PRINTTIMESERIES(ts,0);}

    
    ALLOCATETS(s,m,ts->dimensions);
    /*
      memcpy(s->x,r->ts->x,r->ts->length*sizeof(Xorder));
      memcpy(s->y,r->ts->y,r->ts->length*sizeof(Mat));
      memcpy(s->x+r->ts->length,ts->x,ts->length*sizeof(Xorder));
      memcpy(s->y+r->ts->length,ts->y,ts->length*sizeof(Mat));
    */
    

    copy_ts(s,ts);
    append_ts(s,r->ts);
    

    if (debug) { 
      PRINTTIMESERIES(s,0);
      PRINTF("sorting by time \n");
    }
    SORT_TIMESERIES(s,TIMESLOT);
    if (debug) PRINTTIMESERIES(s,0);
    

    
    if (max < s->length) { 
      ALLOCATETS(g,max,ts->dimensions);
      /* We order the timeseries in decreasing time order 
	 thus we must  store only the first part
	 memcpy(g->x,s->x+s->length-max,max*sizeof(Xorder));
	 memcpy(g->y,s->y+s->length-max,max*sizeof(Mat));
      */
      CROP_TS(g,s,max);
      FREETS(s);
    }
    else {
      
      g = s;
    }
    
    if (debug) PRINTTIMESERIES(g,0);
    res = WindowFromTimeSeries(g);
    
    FREETS(g);
    FREEW(r);
    return res;
  }

}


void setCodeTimeseries(  TimeSeries *ts, int code) { 
  
  int i;
  for (i=0;i<ts->length;i++) { 
    ts->data[i].code = code;
  }



}


Adj *allocate_adj(int N, int dimensions) { 
  
  Adj *f = (Adj*) calloc(1,sizeof(Adj));                   
  assert(f);                                           
  ALLOCATECIRCULAR(f->ts,N,dimensions);          
  f->max  = N;                                       
  f->pdf  = (double*) calloc(N,sizeof(double));      
  f->adj  = (double*) calloc(N*N,sizeof(double));    
  f->mins = (int*) calloc(N,sizeof(int));            
  assert(f->pdf && f->adj && f->mins);           
  return f;
}



