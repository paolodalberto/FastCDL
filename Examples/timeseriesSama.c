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
#include <stdlib.h>
#include <stdio.h>
#include <cdl.h>


#define NONPA 1
#define HWTEST 2
#define MATEST 3

/*
  #include <type.h>
  #include <assert.h>
  #include <distance.h>
  #include <interface.h>
  #include <sort.h>
  #include <quicksort.h>
  #include <stdlib.h>
  #include <stdio.h>
  #include <doubly_compensated_sumc.h>
  #include <pvalue.h>
  #include <string.h>
*/

/* 
   PREV: leds 1 in 1 turned 1 on 1 prospect 1 park 1 holiday 1
   CURR: leds 1 in 1 turned 1 on 1 prospect 1 park 1 holiday 1 


   char *w1[7] = {"leds", "in", "turned", "on", "prospect", "park", "holiday" };
   char *w2[7] = {"leds",  "turned",  "prospect", "in","park", "holiday","on" };
   int n1[7] = {1,2,3,4,1,3,4};
   int n2[7] = {1,1,1,1,1,1,1};
 */


static int debug = 1;

static inline Xorder dateTranform (char *s) { 
  unsigned int  temp =0;
  int debug_1 = debug;
  debug = 0;
  if (debug) printf(" dateTranform %s \n",s); 
  while ((*s != 0)) { 
    if (debug) printf("%c %d \n", *s, (int) *s); 
    if (*s >= '0' && *s <= '9') {
      temp = temp*10 + (unsigned int) (*s-'0');      
      if (debug) printf("%d-",temp);
    }
    s++;
  }
  
  if (debug) printf("%d dateTranform\n",temp);
  debug = debug_1;
  return (Xorder )temp;
}



TimeSeries *readTimeSeries(char *filename) { 

  FILE *fp;
  int N=0;
  int n=1024;
  int i=0;
  char temp[100];
  char X[10], Y[10];
  TimeSeries *t;
  int done = 0;



  if(debug) printf(" File to open %s \n",filename);

  fp = fopen(filename, "r");
  fscanf(fp, "%d\t%s", &N,temp);
  if(debug) printf("%d\t%s\n",N,temp);
  ALLOCATETS(t,N);
  
  while (i<N && (EOF != fscanf(fp, "%s\t%s", X,Y))) {
    
    
    t->x[i] = dateTranform(X);
    t->y[i] = atof(Y);
    if(debug) printf("%s\t%s\t%d\t%e\n", X,Y,t->x[i],t->y[i]);

    i++;

  }
  t->length = N;

  if(debug) printf("Close\n");
  fclose(fp);
  
  return t;
}



int main() { 
  int i,l,j;
  int time;
  int size;
  Mat nonparameters[3] = {16, 16,1};
  Mat Hw[6] = {16, 0.3, 0.2, 0.4, 2, BOTHSIDE};
  Mat Hw2[6] = {30, 0.3, 0.1, 0.2, 2, BOTHSIDE};
  GeneralizedOutput *o[3];
  GeneralizedOutput *oo;
  HoltWinterState *hws=0,*serhw=0;
  MovingAverageState *mas=0,*serma=0;
  NonParametricState *nps=0, *ser=0, *ser2;
  int response[3]= {0, 0,0}; 
  Window     *r, *w, *s,*e1,*e2;
  TimeSeries *tr, *tw, *ts, *ts2;
  unsigned int bytes;
  char *th,*temp;
  double averageduration;
  char filename[100];

  int operation;

  char *permopen = "rb+";
  char *permwrite = "wb+";

  printf("NP %d \n",sizeof(NonParametricState));
  printf("HW %d \n",sizeof(HoltWinterState));
  printf("W %d \n",sizeof(Window));
  printf("TS %d \n",sizeof(TimeSeries));
  printf("SW %d \n",sizeof(StatisticalWindow));
  printf("CB %d \n",sizeof(CircularBuffer));
  printf("SHW %d \n",sizeof(HoltWinterState));
  printf("SNON %d \n",sizeof(NonParametricState));

  printf("int    %d \n",sizeof(int));
  printf("long   %d \n",sizeof(long int));
  printf("float  %d \n",sizeof(float));
  printf("double %d \n",sizeof(double));

  printf("Input file");
  scanf("%s",filename);



  printf("operation NONPA 1/HWTEST 2/BOTH 0\n");
  scanf("%d",&operation);
  
  

  tr = readTimeSeries(filename);
    
  
  nonparameters[0] = nonparameters[1] = 30;
  Hw[0] = 0;//tr->length;
  
  if (!operation || operation ==NONPA) {
    ALLOCATETS(tw,nonparameters[0] );
    ALLOCATETS(ts,tr->length);
    ts->length = tr ->length;
    for (j=0;j<ts->length;j++) {
	  ts->x[j] = tr->x[j];
	  ts->y[j] = tr->y[j];
    }

    tw->length=nonparameters[0];
    nps =0;

    while (ts->length && ts->length>tw->length) {
      
      printf(" Non Parametric methods: quorum/Total > 0.5 pvalue < 0.5 Size %d \n", ts->length);
      PRINTTIMESERIES(ts,0);
      for (i=0;i<ts->length ; i+=nonparameters[0]) {
	tw->length=(i+nonparameters[0]<ts->length)?nonparameters[0]:(ts->length-i);
	for (j=0;j<tw->length;j++) {
	  tw->x[j] = ts->x[i+j];
	  tw->y[j] = ts->y[i+j];
	  
	}
	/*      PRINTTIMESERIES(tw,0);*/
	
	scalarF(tw,NonParametricMethod,(void**)&nps,&oo,nonparameters);
	
	if (PrintGeneralizedOutputVertical(oo)) 
	  PRINTNONPSTATE(nps,0);
	
	
	//PrintGeneralizedOutput(oo);
      }
      FREE_NONP_STATE(nps);
      ALLOCATETS(ts2,ts->length-tw->length);
      ts2 ->length = ts->length-tw->length;
      for (j=0;j<ts2->length;j++) {
	  ts2->x[j] = ts->x[j+tw->length];
	  ts2->y[j] = ts->y[j+tw->length];
      }
      
      FREETS(ts);
      ts = ts2;
    }
    FREETS(tw);
    FREETS(ts);
    FREE_GEN_OUTPUT(oo) ;
    time +=size;
    /*temp = PrintNONPState(nps,0,0);
      free(temp);*/
    temp =0; 
    //    FREE_NONP_STATE(nps);
    
  }  
  
  if (!operation || operation ==HWTEST) {
    
    scalarF(tr,HoltWintersMethod,(void**)&hws,&oo,Hw);
    
    printf(" HoltWinters:  {P=0, alpha=0.3, beta=0.2, gamma=0.4, 2delta, BOTHSIDE};\n");
    PrintGeneralizedOutputVertical(oo);
    FREE_GEN_OUTPUT(oo) ;
    //temp = PrintHWState(hws,0,0);
    //free(temp); temp = 0;
    FREE_HW_STATE(hws);

  }
  
  if (!operation || operation ==MATEST) {
    Hw[KPLACE] = 2.9;
    Hw[PPLACE] = 3;
    

    scalarF(tr,MovingAverageMethod,(void**)&mas,&oo,Hw);
    printf(" Moving average:  {P=3, 3delta, BOTHSIDE};\n");
    PrintGeneralizedOutputVertical(oo);
    FREE_GEN_OUTPUT(oo) ;
    FREE_MA_STATE(mas);
   

  }

    FREETS(tr);


  return 0;
}
