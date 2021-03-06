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
Histogram *eh1,*eh2;
  
DistributionFunction *df, *sampledf;

Mat *pdf1, *pdf2, *df1, *df2;
Mat *res;

static int debug = 1;

char *randomWord(int l,char *res) { 

  int i;
  char c;
  
  assert(res);
  for (i=0;i<l;i++) { 
    c = (char) (((char)(rand()%30))+'A');
    res[i]=c;
  }
  res[i]='\0';
  return res;

}

Histogram *randomHistogram(int l) { 

  Histogram *h = (Histogram *)malloc(sizeof(Histogram));
  int i;
  int wordlength;
  char *res;
  assert(h);

  h->word    = (char **) malloc(l*sizeof(char **));
  h->number  = (Mat *) malloc(l*sizeof(Mat));
  h->length = l;

  assert(h->word && h->number);

  for (i=0;i<l;i++) { 
    wordlength = rand()%10 + 1;
    res = (char *) malloc(sizeof(char)*(wordlength+1));
    h->word[i] = randomWord(wordlength,res);
    h->number[i] = drand48();
    printf(" word  %s # %.2f %d - i %d\n", h->word[i],h->number[i],wordlength,i );


  }

  return h;
}


HistogramNumericLabel *randomHistogramNumericLabel(int l) { 

  HistogramNumericLabel *h = (HistogramNumericLabel *)malloc(sizeof(HistogramNumericLabel));
  int i;
  int wordlength;
  char *res;
  assert(h);

  h->word    = (Mat *) malloc(l*sizeof(Mat));
  h->number  = (Mat *) malloc(l*sizeof(Mat));
  h->length = l;

  assert(h->word && h->number);

  for (i=0;i<l;i++) { 
    h->word[i] = i;
    h->number[i] = drand48();
    printf(" word  %e # %.2f  - i %d\n", h->word[i],h->number[i],i );


  }

  return h;
}




int main() { 
  int i,l;
  double time;
  int samples;
  HistogramNumericLabel *hn1,*hn2;
  
  
  
  eh1 = (Histogram *) malloc(sizeof(Histogram)); 
  eh2 = (Histogram *) malloc(sizeof(Histogram)); 
  h1 = (Histogram *) malloc(sizeof(Histogram)); 
  h2 = (Histogram *) malloc(sizeof(Histogram)); 


  h1->word   = w1;
  h1->number = n1;
  h1->length = 4;

  h2->word   = w2;
  h2->number = n2;
  h2->length = 4;


  printHistogram(h1);
  printHistogram(h2);

  SORT_HISTOGRAM(h1);
  SORT_HISTOGRAM(h2);

  
  printHistogram(h1);
  printHistogram(h2);
  
  printf("extension histogram \n");  
  extendHistogram(h1,h2,eh1,eh2);
  printHistogram(eh1);
  printHistogram(eh2);


  
  pdf1 =  ProbabilityDistributionFromHistogram(eh1->number,eh1->length);
  pdf2 =  ProbabilityDistributionFromHistogram(eh2->number,eh2->length);

  printPDF_DFHistogram(pdf1,eh1->length);
  printPDF_DFHistogram(pdf2,eh2->length);

  df1 =  DistributionFunctionFromHistogram(eh1->number,eh1->length,1);
  printPDF_DFHistogram(df1,eh1->length);  
  df2 =  DistributionFunctionFromProbabilityDistribution(pdf2,eh2->length,1);
  printPDF_DFHistogram(df2,eh2->length);

  res = (Mat *)calloc(DIM*(FNumber+FParNumber),sizeof(double));
   
  res = distanceMeasure(df1,df2,eh2->length,
			res,
			F,FNumber,
			FP,PAR,FParNumber);

  PRINTRESULTDISTANCE(res,res+FNumber+FParNumber);




  free(res);
  free(pdf1);
  free(pdf2);
  free(df1);
  free(df2);
  free(eh1);
  free(eh2);
  free(h1);
  free(h2);

  
  printf("second test: introduce the number of words \n");
  scanf("%d",&l);
  printf("\n Length %d \n",l);

  printf("Gen h1 \n");
  h1 = randomHistogram(l);
  if (debug) printHistogram(h1);
  printf("Gen h1 \n");
  h2 = randomHistogram(l);
  if (debug) printHistogram(h2);

  printf("Dist h1~h2 \n");
  

  res = distanceHistograms(h1,h2);
  //  TIMING((res = distanceHistograms(h1,h2)),time,5);
  //printf("two histogram comparison (18 distance) in %f seconds \n", time);
  //printf("an average of %f per distance function \n", time/18);

  PRINTRESULTDISTANCE(res,res+FNumber+FParNumber);


  // error  PRINTRESULTDISTANCE(res->y,res->pvalue);

  // the word array is shared with the histogram
  df = DFFromHist(h1);
  printDF(df);
  
  printf("Sample? \n");
  scanf("%d",&samples);

  // nothing is sahred 
  sampledf = SampleDistribution(df,samples);
  if (sampledf) printDF(sampledf);
  
  printf("free sampledf\n");
  freeDistributionContents(sampledf);

  printf("free df\n");
  FREE_DF(df);
  
  free(res);
  printf("free h1\n");
  freeHistogramContents(h1);  
  freeHistogramContents(h2);  


  hn1 = randomHistogramNumericLabel(l);
  hn2 = randomHistogramNumericLabel(l);
  
  res = distanceHistogramsQQNumericLabel(hn1,hn2);
  
  printf("%e %e \n",res[0],res[1]);

  FREE_EXTENDED_HIST(hn1);
  FREE_EXTENDED_HIST(hn2);
  
  free(res);
  return 0;
}
