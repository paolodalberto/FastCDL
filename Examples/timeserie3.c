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
#include <stdlib.h>
#include <stdio.h>
#include <cdl.h>

Mat input[120] = {  
  0, 0, 0, 0, 0,1,2,3,4, 5, 6, 7, 8, 9, 10,11,12,13,14,15,16,17,18,19,20,21, 22,23,24, 0, 
  0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,  16,17,18,19,20,21,22,23,24, 0, 
  0, 0,  0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,  10,11,12,13,14,15,16,17,18,19,20,21,  22,23,24, 0, 
  0, 0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,  14, 15, 16, 17, 18, 19, 20, 21, 22, 23,  24,  0 };

#define NONPA 1
#define HWTEST 2

int operation;

TimeSeries *randomTimeseries(int N, int start , int k) { 

  int i;
  TimeSeries *t;

  ALLOCATETS(t,N);

  for (i=0;i<N;i++) { 
    t->x[i] = start+i;  
    if ((k%2)==0) 
      t->y[i] = (Mat) drand48();
    else {
      t->y[i] = (Mat) drand48()+(Mat) drand48() ;
     
    }
  }
  t->length = N;
  return t;
} 



char *readfile(FILE *F) { 

  int size = 1024;
  char *temp = (char*) calloc(size,sizeof(char));
  int c,i=0;
  
  while ((c = fgetc(F)) != EOF) {
    
    if (i<size)  {temp[i]=(char) c; }
    else {  
      size = 2*size;
      char *k =  (char*) calloc(size,sizeof(char));
      strcpy(k,temp);
      free(temp);
      temp = k;
      temp[i]=(char) c;
    }
    i++;
  }

  return temp;
}

int length(char *t) { 
  int i=0;
  while (t[i]!=0) { i++;} 

  return i+1;

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
  NonParametricState *nps=0, *ser=0, *ser2;
  int response[3]= {0, 0,0}; 
  Window     *r, *w, *s,*e1,*e2;
  TimeSeries *tr, *tw, *ts;
  FILE *stateN, *stateHW;
  unsigned int bytes;
  char *th,*temp;
  double averageduration;

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


  printf("operation NONPA 1/HWTEST 2/BOTH 0\n");
  scanf("%d",&operation);

  
  printf("P\n");
  scanf("%d",&size);
  
  nonparameters[0] = nonparameters[1] = size;
  Hw[0] = size;
  
  srand48(3);
  printf("time\n");
  scanf("%d",&i);
  
  // tr = randomTimeseries(size,0);
  
  if (!operation || operation ==NONPA) {
    
    int time=0;
    
    for (j=0;j<i;j++) { 
      
      tr = randomTimeseries(size,time,j);
      PRINTTIMESERIES(tr,0);
      scalarF(tr,NonParametricMethod,(void**)&nps,&oo,nonparameters);

      PRINTNONPSTATE(nps,0);
      PrintGeneralizedOutput(oo);
      FREETS(tr);
      FREE_GEN_OUTPUT(oo) ;
      time +=size;
    }
    tr = randomTimeseries(size,time,j);
    /*    TIMING(scalarF(tr,NonParametricMethod,(void**)&nps,&oo,nonparameters),
	   averageduration, 
	   10);*/
    PrintGeneralizedOutput(oo);
    PRINTNONPSTATE(nps,0);
    

    FREETS(tr);
    FREE_GEN_OUTPUT(oo) ;

    temp = PrintNONPState(nps,0,0);
    
  
    
    free(temp);
    temp =0; 
    FREE_NONP_STATE(nps);
    
  }  
  
  if (!operation || operation ==HWTEST) {
    int time=0;
 
    
    for (j=0;j<i;j++) { 
      tr = randomTimeseries(size,time,j); 
      PRINTTIMESERIES(tr,0);
      scalarF(tr,HoltWintersMethod,(void**)&hws,&oo,Hw);
      FREETS(tr);
      
      PrintGeneralizedOutput(oo);
      FREE_GEN_OUTPUT(oo) ;
      time +=size;

    }

    temp = PrintHWState(hws,0,0);
    
    stateHW  = fopen("c.bin",permwrite);
    fwrite(temp,sizeof(char),length(temp),stateHW);
    fclose(stateHW);
    
    free(temp); temp = 0;
    FREE_HW_STATE(hws);
   




  }
  



  return 0;
}
