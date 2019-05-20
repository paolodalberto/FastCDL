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
#define GETTIME 1
#include <type.h>
#include <assert.h>
#include <distance.h>
#include <interface.h>
#include <sort.h>
#include <quicksort.h>
#include <stdlib.h>
#include <stdio.h>
#include <doubly_compensated_sumc.h>
#include <timeseries.h>
#include <holtwinters.h>
#include <nonparametric.h>
#include <window.h>

TimeSeries *randomTimeseries(int N, int start) { 

  int i;
  TimeSeries *t;

  ALLOCATETS(t,N);

  for (i=0;i<N;i++) { 
    t->x[i] = start+i;
    t->y[i] = (Mat) drand48();
  }
  t->length = N;
  return t;
} 


int main() { 
  int i,l,j;
  double time;
  int size;
  Mat nonparameters[3] = {10, 10,1};
  Mat Hw[4] = {10, 0.3, 0.2, 0.4};
  GeneralizedOutput *o[3];
  GeneralizedOutput *oo;
  HoltWinterState *hws=0;
  NonParametricState *nps=0;
  int response[3]= {0, 0,0}; 
  Window     *r, *w, *s,*e1,*e2,*wtr;
  TimeSeries *tr, *tw, *ts, *mtr;
  char *temps,*temps2;
  e1 = e2 = 0;

  scanf("%d",&size);
  srand48(3);

  tr = randomTimeseries(size,0);
 
  temps = PrintTimeseries(tr,0,1);
  temps2 = temps;
  mtr   = ReadTimeseries(0,&temps2);
  PRINTTIMESERIES(mtr,0);
  
  free(mtr);
  free(temps);



  tw = randomTimeseries(10,20);
  // PrintTimeseries(tw,0);
  ts = randomTimeseries(10,30);
  //PrintTimeseries(ts,0);
  

  r = WindowFromTimeSeries(tr);
  if (!verify(r->sw->sortedts->y,0,r->sw->sortedts->length)) { 
    printf("ERROR \n");
  }

  PRINTWINDOW(r,0);



  w = WindowFromTimeSeries(tw);
  


  //s = WindowFromTimeSeries(ts);
  //PrintWindow(w);
  
  temps = PrintWindow(w,0,1);
  temps2 = temps;
  wtr = ReadWindow(0,&temps2);
  PRINTWINDOW(wtr,0);

  
  r = WindowUpdate(r,ts,r->ts->length);
  PRINTWINDOW(r,0);
  
  e1= (Window *) malloc(sizeof(Window));    
  assert(e1);
  e2= (Window *) malloc(sizeof(Window));
  assert(e2);

  

    
  printf("extension  \n");
  
  extendWindow(r,w,e1,e2,VALUESLOT);
  printf("r extended  \n");
  PRINTWINDOW(e1,3);
  printf("w extended  \n");
  PRINTWINDOW(e2,3);
  
  FREEW(r);
  FREEW(w);
  FREE_EW(e1,e2);
  
  
  /*
  
  response[0] = scalarF(tw,HoltWintersMethod,(void**)&hws,&o[0],Hw);
  PrintGeneralizedOutput(o[0]);

  response[1] = scalarF(tr,HoltWintersMethod,(void**)&hws,&o[1],Hw);
  PrintGeneralizedOutput(o[1]);
  response[2] = scalarF(ts,HoltWintersMethod,(void**)&hws,&o[2],Hw);
  PrintGeneralizedOutput(o[2]);
  FREE_HW_STATE(hws);
  FREE_GEN_OUTPUT_P(o[0]); 
  FREE_GEN_OUTPUT_P(o[1]) ;
  FREE_GEN_OUTPUT_P(o[1]) ;

  response[0] = scalarF(tw,NonParametricMethod,(void**)&nps,&o[0],nonparameters);
  PrintGeneralizedOutput(o[0]);
  
  response[1] = scalarF(tr,NonParametricMethod,(void**)&nps,&o[1],nonparameters);
  PrintGeneralizedOutput(o[1]);
  response[2] = scalarF(ts,NonParametricMethod,(void**)&nps,&o[2],nonparameters);
  PrintGeneralizedOutput(o[2]);
  FREE_GEN_OUTPUT_P(o[0]); 
  FREE_GEN_OUTPUT_P(o[1]); 
  FREE_GEN_OUTPUT_P(o[2]) ;
  FREETS(tr);
  FREETS(tw);
  FREETS(ts);
  
  
  printf("time\n");
  scanf("%d",&i);

  for (j=0;j<i;j++) { 
    tr = randomTimeseries(size,0);
    scalarF(tr,NonParametricMethod,(void**)&nps,&oo,nonparameters);
    FREETS(tr);
    FREE_GEN_OUTPUT(oo) ;
  }
  
  FREE_NONP_STATE(nps);

  for (j=0;j<i;j++) { 
    tr = randomTimeseries(size,0);
    scalarF(tr,HoltWintersMethod,(void**)&hws,&oo,Hw);
    FREETS(tr);
    FREE_GEN_OUTPUT(oo) ;
  }
  
  FREE_HW_STATE(hws);
  */


  return 0;
}
