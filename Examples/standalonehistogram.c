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
#include <pvalue.h>
#include <string.h>
/* 
 PREV: leds 1 in 1 turned 1 on 1 prospect 1 park 1 holiday 1
 CURR: leds 1 in 1 turned 1 on 1 prospect 1 park 1 holiday 1 


char *w1[7] = {"leds", "in", "turned", "on", "prospect", "park", "holiday" };
char *w2[7] = {"leds",  "turned",  "prospect", "in","park", "holiday","on" };
int n1[7] = {1,2,3,4,1,3,4};
int n2[7] = {1,1,1,1,1,1,1};
 */


char *w1[4] = {"leds", "in", "turned", "on"  };
char *w2[4] = {"leds", "in", "turned", "on"  };
Mat n1[4] = {0.4, 0.7, 0.9, 0.11};
Mat n2[4] = {0.3, 0.6, 0.8, 0.10};



Histogram *h1,*h2;

  

Mat *res;

static int debug = 1;

Histogram *readHistogram(char *filename) { 

  FILE *fp;
  int N=0;
  int i=0;
  Histogram *h;
  char temp[100];
  char *que;
  if(debug) printf(" File to open %s \n",filename);
  
  h = (Histogram *) calloc(1,sizeof(Histogram));

  fp = fopen(filename, "r");

  if (fscanf(fp, "%s\t%d", temp,&N)==EOF ||  N==0)
    return 0;
    
  
  if(debug) printf("%s\t%d\n", temp,N);
  h->word    = (char **) malloc(N*sizeof(char **));
  h->number  = (Mat *) malloc(N*sizeof(Mat));      
  h->length  = N;                                  
 
  while ((i< h->length) && (EOF != fscanf(fp, "%d\t%s", &N,temp))) {
    if(debug) printf("%d\t%d\t%s\n", i,N,temp);
    que = (char *) malloc(100*sizeof(char));




    assert(que);
    strcpy(que,temp);
    h->word[i]   = que;
    h->number[i] = N;
    i++;
  }

  if(debug) printf("Close\n", i,N,temp);
  fclose(fp);
  
  return h;
}




typedef double* ( * DH)(Histogram *,Histogram *);



int main(int argc, char** argv) { 
  int i,l;
  double time;
  int samples;
  DH dh [2] =  { distanceHistogramsQ, distanceHistograms} ;


  if(debug) printf("%s %s %s \n",argv[0],argv[1],argv[2]);

  h1  = readHistogram(argv[1]);
  h2 = readHistogram(argv[2]);

  if (h1 && h2 ) { 


    for (l=0; l<2; l++) {
      res = dh[l](h1,h2);  
      if ( dh[l] == distanceHistograms) { 
	PRINTRESULTDISTANCE(res,res+N_METHODS);
      }
      else { 
	printf("min dist = %f, pvalue = %f \n", res[0],res[1]);
      }

      free(res);
    }
    freeHistogramContents(h1);  
    freeHistogramContents(h2);  
    //free(h1);
    //free(h2);

  }
  else {

    if(debug && !h1) printf("%s empty\n",argv[1]);
    if(debug && !h2) printf("%s empty\n",argv[2]);


  }
  

  
  return 0;
}
