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
#include <cdl.h>


#include <stdlib.h>
#include <stdio.h>
#include <math.h>

Mat timeT[1555]  = {
  1,5,3,1,2,4,6,2,5,3,1,4,2,6,3,5,3,1
,2,4,6,4,5,3,1,4,2,6,5,5,3,1,4,2,6,6
,5,3,1,4,2,6,7,5,3,1,2,4,6,8,5,3,1,2
,4,6,9,5,3,1,2,4,6,10,5,3,1,4,2,6,11,5
,3,1,2,6,4,12,5,1,3,4,2,6,13,5,3,1,4,2
,6,14,5,3,1,4,2,6,15,5,3,1,4,2,6,16,5,3
,1,4,2,6,17,5,3,1,4,2,6,18,5,3,1,4,2,6
,19,5,3,1,4,2,6,20,5,3,1,2,4,6,21,5,1,3
,2,4,6,22,5,1,3,2,4,6,23,5,3,1,2,4,6,24
,5,3,1,2,4,6,25,5,3,1,2,4,6,26,5,3,1,2
,4,6,27,5,3,1,2,4,6,28,5,3,1,2,4,6,29,5
,1,3,2,4,6,30,5,3,1,2,4,6,31,5,3,1,2,4
,6,32,5,3,1,2,4,6,33,5,3,1,2,4,6,34,5,3
,1,2,4,6,35,5,3,1,2,4,6,36,5,3,1,2,4,6
,37,5,3,1,2,4,6,38,5,3,1,2,4,6,39,5,1,3
,2,4,6,40,5,3,1,2,4,6,41,5,3,1,2,4,6,42
,5,3,1,2,4,6,43,5,3,1,2,4,6,44,5,3,1,2
,4,6,45,5,3,1,2,4,6,46,5,3,1,2,4,6,47,5
,1,3,2,4,6,48,5,1,3,2,4,6,49,5,3,1,2,4
,6,50,5,3,1,2,4,6,51,5,3,1,2,4,6,52,5,3
,1,2,4,6,53,5,3,1,2,4,6,54,5,3,1,2,4,6
,55,5,1,3,2,4,6,56,5,3,1,2,4,6,57,5,3,1
,2,4,6,58,5,3,1,2,4,6,59,5,3,1,2,4,6,60
,5,3,1,2,4,6,61,1,2,3,4,5,6,62,5,3,1,2
,4,6,63,5,3,1,4,2,6,64,5,3,1,2,4,6,65,5
,3,1,4,2,6,66,5,3,1,4,2,6,67,5,3,1,4,2
,6,68,5,3,1,2,4,6,69,5,3,1,2,4,6,70,5,3
,1,2,4,6,71,5,3,1,4,2,6,72,5,3,1,2,6,4
,73,5,1,3,4,2,6,74,5,3,1,4,2,6,75,5,3,1
,4,2,6,76,5,3,1,4,2,6,77,5,3,1,4,2,6,78
,5,3,1,4,2,6,79,5,3,1,4,2,6,80,5,3,1,4
,2,6,81,5,3,1,2,4,6,82,5,1,3,2,4,6,83,5
,1,3,2,4,6,84,5,3,1,2,4,6,85,5,3,1,2,4
,6,86,5,3,1,2,4,6,87,5,3,1,2,4,6,88,5,3
,1,2,4,6,89,5,3,1,2,4,6,90,5,1,3,2,4,6
,91,5,3,1,2,4,6,92,5,3,1,2,4,6,93,5,3,1
,2,4,6,94,5,3,1,2,4,6,95,5,3,1,2,4,6,96
,5,3,1,2,4,6,97,5,3,1,2,4,6,98,5,3,1,2
,4,6,99,5,3,1,2,4,6,100,5,1,3,2,4,6,101,5
,3,1,2,4,6,102,5,3,1,2,4,6,103,5,3,1,2,4
,6,104,5,3,1,2,4,6,105,5,3,1,2,4,6,106,5,3
,1,2,4,6,107,5,3,1,2,4,6,108,5,1,3,2,4,6
,109,5,1,3,2,4,6,110,5,3,1,2,4,6,111,5,3,1
,2,4,6,112,5,3,1,2,4,6,113,5,3,1,2,4,6,114
,5,3,1,2,4,6,115,5,3,1,2,4,6,116,5,1,3,2
,4,6,117,5,3,1,2,4,6,118,5,3,1,2,4,6,119,5
,3,1,2,4,6,120,5,3,1,2,4,6,121,5,3,1,2,4
,6,122,1,2,3,4,5,6,123,1,5,2,6,3,4,124,1,5
,2,6,3,4,125,1,2,3,4,5,6,126,1,2,3,4,5,6
,127,1,2,3,4,5,6,128,1,2,3,4,5,6,129,1,2,5
,3,6,4,130,1,2,5,3,6,4,131,1,2,5,3,6,4,132
,1,2,5,3,6,4,133,1,2,3,5,6,4,134,1,2,5,3
,6,4,135,1,2,5,3,6,4,136,1,2,5,3,6,4,137,1
,4,2,3,5,6,138,1,2,3,5,6,4,139,1,2,3,5,4
,6,140,1,4,2,3,5,6,141,1,6,2,3,5,4,142,1,4
,2,6,3,5,143,1,6,2,4,3,5,144,1,4,2,6,3,5
,145,1,4,2,6,3,5,146,1,4,2,6,3,5,147,1,4,2
,6,3,5,148,1,4,2,6,3,5,149,1,4,2,6,3,5,150
,1,4,6,2,3,5,151,1,4,2,6,3,5,152,1,2,4,6
,3,5,153,1,2,6,3,5,4,154,1,2,6,3,5,4,155,1
,4,2,6,3,5,156,1,2,6,3,5,4,157,1,2,6,3,5
,4,158,1,2,6,3,5,4,159,1,4,2,6,3,5,160,1,2
,6,3,5,4,161,1,2,6,3,5,4,162,1,2,6,3,5,4
,163,1,2,6,3,5,4,164,1,6,2,3,5,4,165,1,2,6
,3,5,4,166,1,4,2,6,3,5,167,1,2,6,3,5,4,168
,1,2,6,3,5,4,169,1,2,6,3,5,4,170,1,2,6,3
,5,4,171,1,2,6,3,5,4,172,1,2,6,3,5,4,173,1
,4,2,6,3,5,174,1,2,6,3,5,4,175,1,2,6,3,5
,4,176,1,2,6,3,5,4,177,1,2,6,3,4,5,178,1,2
,6,3,5,4,179,1,2,6,3,5,4,180,1,2,6,3,5,4
,181,1,2,6,3,5,4,182,1,2,6,3,5,4,183,1,2,6
,3,5,4,184,1,2,6,3,5,4,185,1,2,6,3,5,4,186
,1,2,6,3,4,5,187,1,2,6,3,5,4,188,1,2,6,3
,4,5,189,1,6,2,3,5,4,190,1,4,2,5,6,3,191,1
,4,5,2,6,3,192,1,5,2,6,3,4,193,1,2,6,3,4
,5,194,1,2,6,3,4,5,195,1,4,2,5,6,3,196,1,2
,6,3,4,5,197,1,2,6,3,4,5,198,1,2,6,3,4,5
,199,1,2,6,3,4,5,200,1,2,6,3,4,5,201,1,2,6
,3,4,5,202,1,4,2,5,6,3,203,1,2,6,3,4,5,204
,1,2,6,3,4,5,205,1,2,6,3,4,5,206,1,2,6,3
,4,5,207,1,2,6,3,4,5,208,1,2,6,3,4,5,209,1
,2,6,3,4,5,210,1,2,6,3,4,5,211,1,2,6,3,4
,5,212,1,2,6,3,4,5,213,1,5,2,6,3,4,214,1,5
,2,6,3,4,215,1,2,6,3,4,5,216,1,2,6,3,4,5
,217,1,2,6,3,4,5,218,1,2,6,3,4,5,219,1,5,2
,6,3,4,220,1,5,2,6,3,4,221,1,4,2,6,3,5,222
  ,1,2,6,3,4,5};

TimeSeries *randomTimeseries3() { 
  static int j=0;
  int i,l;
  TimeSeries *t;
  int N = 222;
  int k= 6;

  ALLOCATETS(t,N,k);

  for (i=0;i<N;i++) { 
    t->data[i].x = i;  
    for (l=0;l<k;l++)
      t->data[i].y[l] = (Mat) timeT[(k+1)*i+l+1] /*+j*((l+1)%(1)>0)j*5*(1+sin(2*pi*(i+j)/N))*/;
  }
  t->length = N;
  j += 2;
  return t;
} 



Mat input[120] = {  
  0, 0, 0, 0, 0,1,2,3,4, 5, 6, 7, 8, 9, 10,11,12,13,14,15,16,17,18,19,20,21, 22,23,24, 0, 
  0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,  16,17,18,19,20,21,22,23,24, 0, 
  0, 0,  0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,  10,11,12,13,14,15,16,17,18,19,20,21,  22,23,24, 0, 
  0, 0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,  14, 15, 16, 17, 18, 19, 20, 21, 22, 23,  24,  0 };

#define NONPA 1
#define HWTEST 2
#define MATEST 3
#define MARTINGALETEST 4
#define KOLMOGOROVTEST 5
#define KERNELMMD      6
#define pi     3.1462
int operation;

TimeSeries *randomTimeseries(int N, int start , int k) { 
  static int j=0;
  int i,l;
  TimeSeries *t;


  ALLOCATETS(t,N,k);


  for (i=0;i<N;i++) { 
    t->data[i].x = start+i;  
    for (l=0;l<k;l++)
      t->data[i].y[l] =  rand() % 10/*j*((l+1)%(1)>0)/*j*5*(1+sin(2*pi*(i+j)/N))*/;
  }
  t->length = N;
  j += 2;
  return t;
} 


Mat *randomSeries(int N, int start , int k, Xorder *x) { 
  static int j=0;
  int i,l;
  Mat *t;


  printf("Random Series N %d  k %d \n",N,k);
  
  t = (Mat *) malloc(N*k*sizeof(Mat));
  assert(t);
  for (i=0;i<N;i++) { 
    x[i] = start+i;  
    for (l=0;l<k;l++)
      t[i*k + l] = rand()% 10 /*j*5*(1+sin(2*pi*(i+j)/N))*/;
  }

  j += 2;
  return t;
} 
Mat *randomSeries3(int N, int k, Xorder *x) { 
  static int j=0;
  int i,l;
  Mat *t;
  //  N = 222;
  //k = 6;

  printf("Random Series N %d  k %d \n",N,k);
  
  t = (Mat *) malloc(N*k*sizeof(Mat));
  assert(t);
  for (i=0;i<N;i++) { 
    x[i] = timeT[(k+1)*i] ;  
    for (l=0;l<k;l++)
      t[i*k + l] = (Mat) timeT[(k+1)*i+l+1] /*j*5*(1+sin(2*pi*(i+j)/N))*/;
  }

  j += 2;
  return t;
} 


int length(char *t) { 
  int i=0;
  while (t[i]!=0) { i++;} 

  return i+1;

}

int main() { 
  int i,l,j;
  double time;
  int size;
  int debug =0;
  int dimensions;
  

  Mat kernelpar[5]     = {16, 16, 1, 1, 0};
  Mat nonparameters[6] = {16, 16,1,0.5,0,1};
  Mat Mar[6] = {0.92, 3, 20, 0.5, 400, 0  };
  Mat Hw[6] = {0, 0.3, 0.2, 0.4, 2, BOTHSIDE};
  Mat Hw2[6] = {30, 0.3, 0.1, 0.2, 3, BOTHSIDE};
  GeneralizedOutput *o[3];
  GeneralizedOutput *oo;
  MovingAverageState *mas=0,*serma=0;
  HoltWinterState *hws=0,*serhw=0;
  NonParametricState *nps=0, *ser=0, *ser2;
  KolmogorovState *ks=0;
  MartingaleState *marts=0;
  Xorder *x;
  double *yout,*pvalue,*ym,*dm;

  timestamp *xout;
  int response[3]= {0, 0,0}; 
  Window     *r, *w, *s,*e1,*e2;
  TimeSeries *tr, *tw, *ts;
  FILE *stateN, *stateHW,*stateMA,*stateMartingale;
  unsigned int bytes;
  char *th,*temp;

  char *permopen = "rb+";
  char *permwrite = "wb+";

  printf("NP %lu \n",sizeof(NonParametricState));
  printf("HW %lu \n",sizeof(HoltWinterState));
  printf("W %lu \n",sizeof(Window));
  printf("TS %lu \n",sizeof(TimeSeries));
  printf("SW %lu \n",sizeof(StatisticalWindow));
  printf("CB %lu \n",sizeof(CircularBuffer));
  printf("SHW %lu \n",sizeof(HoltWinterState));
  printf("SNON %lu \n",sizeof(NonParametricState));

  printf("int    %lu \n",sizeof(int));
  printf("long   %lu \n",sizeof(long int));
  printf("float  %lu \n",sizeof(float));
  printf("double %lu \n",sizeof(double));


  printf("operation NONPA 1/HWTEST 2/Moving average 3/ Martingale 4/ Compression 5/ Kernel MMD 6/ ALL 0\n");
  scanf("%d",&operation);

  printf("Dimensions\n");
  scanf("%d",&dimensions);
  
  if (dimensions<1) dimensions = 2;

  printf("P\n");
  scanf("%d",&size);
  
  Mar[4] = nonparameters[0] = nonparameters[1] = size/2;
  Mar[5] = 1;
  //  Hw[0] = size;
  


  
  xout = (timestamp*) calloc(size,sizeof(timestamp));
  x = (Xorder*) calloc(size,sizeof(Xorder));
  assert(xout && x);
  
  yout   = (Mat*) calloc(size,sizeof(Mat));
  pvalue = (Mat*) calloc(size,sizeof(Mat));
  assert(yout && pvalue);
  switch (operation)  {
  case MATEST:
  case HWTEST:  
    ym = (Mat *) calloc(2*dimensions*size,sizeof(Mat));
    
    dm = ym   +  size*dimensions;
    assert(ym);
    break;
  default:
    ym = (Mat *) calloc(2*size,sizeof(Mat));
    dm = ym   +  size;
    assert(yout);
    
    break;
  }

  
  srand48(3);
  printf("times\n");
  scanf("%d",&i);
  
  // tr = randomTimeseries(size,0);
  
  if (!operation || operation ==MARTINGALETEST) {
    printf(" Martingale %d\n",operation);
    
    int time=0;
    nonparameters[0] = nonparameters[1] = size;
    for (j=0;j<i;j++) { 
      int method = MartingaleMethod;
      Mar[3] = size/8;

      char *state = 0;
      int lenpar = 6;

      int outn;
	
      Mat *tr = randomSeries(size,time,dimensions,x);
      
      scalarFBASIC(x,tr,&size,&dimensions,
		   &method,
		   Mar, &lenpar,
		   xout, yout,pvalue,ym,dm,&outn, &state);
      
      free(state);
      free(tr);

    }
    
        


    temp = PrintMartingaleState(marts,0,0);
    
    free(temp);
    temp =0; 
    FREE_MARTINGALE_STATE(marts);
    
  }  

  if (!operation || operation ==NONPA) {
    int topological;
    printf(" NONPA %d\n",operation);
    printf("order: MST 0/ POSET 1\n");
    scanf("%d",&topological);
    nonparameters[4] = topological;
    printf(" NONPA %d\n",operation);
    nonparameters[3] = 0.5;
    nonparameters[0] =  nonparameters[1] = 61;
    int time=0;
    char *state = 0;
    for (j=0;j<i;j++) { 
      int method = NonParametricMethod;

      int lenpar = 6;
      
      int outn;
      
      Mat *tr = randomSeries3(size,dimensions,x);

      
      scalarFBASIC(x,tr,&size,&dimensions,
		   &method,
		   nonparameters, &lenpar,
		   xout, yout,pvalue,ym,dm,&outn, &state);




      free(tr);
      //scalarF(tr,NonParametricMethod,(void**)&nps,&oo,nonparameters);
      
      time +=size;
    }
    
    free(state);
  }  
  if (!operation || operation ==KOLMOGOROVTEST) {
    char *state = 0;
    int lenpar = 4;
    int method = CompressionMethod;
    int time=0;
    int outn;
    nonparameters[0] = nonparameters[1] = size/4;
    nonparameters[3] = size/4;
	
    printf(" KOL %d\n",operation);
    for (j=0;j<i;j++) { 
      
      
      Mat *tr = randomSeries(size,time,dimensions,x);

      
      scalarFBASIC(x,tr,&size,&dimensions,
		   &method,
		   nonparameters, &lenpar,
		   xout, yout,pvalue,ym,dm,&outn, &state);



      free(tr);
    }
    

    free(state);
    time +=size;
    
  }  
  
  if (!operation || operation ==HWTEST) {
    int time=0;
    printf(" HWTEST %d\n",operation);
    int method = HoltWintersMethod;
    char *state = 0;
    int outn;
    int lenpar = 6;
    for (j=0;j<i;j++) { 
      
      Mat *tr = randomSeries(size,time,dimensions,x);
      
      
      scalarFBASIC(x,tr,&size,&dimensions,
		   &method,
		   Hw, &lenpar,
		   xout, yout,pvalue,ym,dm,&outn, &state);
      free(tr);
      time +=size;

    }

    free(state);

  }

  Hw[KPLACE] = 3;
  if (!operation || operation ==MATEST) {
    int time=0;
    int method = MovingAverageMethod;
    char *state = 0;
    int outn;
    int lenpar = 6;
    printf(" MATEST %d\n",operation);
    
    for (j=0;j<i;j++) { 
      
      Mat *tr = randomSeries(size,time,dimensions,x);
      
      
      scalarFBASIC(x,tr,&size,&dimensions,
		   &method,
		   Hw, &lenpar,
		   xout, yout,pvalue,ym,dm,&outn, &state);
      free(tr);
      time +=size;

    }

    free(state);

   

  }

  if (!operation || operation ==KERNELMMD) {

    char *state = 0;
    int lenpar = 5;
    int method = KernelMethodMMD;
    int time=0;
    int outn;
    printf(" KERN %d\n",operation);
    kernelpar[0] = size/2 +4;
    kernelpar[1] =   size/2 -4;
    
    for (j=0;j<i;j++) { 
      
      Mat *tr = randomSeries(size,time,dimensions,x);
      
      
      scalarFBASIC(x,tr,&size,&dimensions,
		   &method,
		   kernelpar, &lenpar,
		   xout, yout,pvalue,ym,dm,&outn, &state);
      free(tr);
      time +=size;

    }
    
    free(state);
    
  }  
  
  free(x);
  free(yout);
  free(xout);
  free(pvalue);
  free(ym);

  return 0;
}
