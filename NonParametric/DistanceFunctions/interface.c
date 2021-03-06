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
#define INTERFACE_MODULE 1

static int debug = 0;


#include <stdio.h>
#include <interface.h>
#include <distance.h>
#include <assert.h>
#include <stdlib.h>
#include <sort.h>
#include <quicksort.h>
#include <doubly_compensated_sumc.h>
#include <pvalue.h>
#include <string.h>





void printHistogram(Histogram *h) {

  int i;
  printf(" ------ histogram \n");
  for (i=0;i<(h->length);i++) { 
    printf(" %20s %.2f \n",h->word[i],h->number[i]);
  }
}
void printDF(DistributionFunction *h) {

  int i;
  printf(" ------ Distribution \n");
  for (i=0;i<(h->length);i++) { 
    printf(" %20s %1.5f \n",h->word[i],h->number[i]);
  }
}

void printPDF_DFHistogram(Mat *h, int N) {

  int i;
  printf(" ------ pdf - df \n");
  for (i=0;i<N;i++) { 
    printf("%e ",h[i]);
  }
  printf("\n");
}


void freeHistogramContents(Histogram *h) { 

  int i;
  if (h && h->word && h->number) {
    for (i=h->length-1;i>=0;i--) { 
      if (h->word[i]) 
	free(h->word[i]);
    }
    free(h->word);
    free(h->number);
    
    h->word = 0;
    h->number = 0;
    h->length=0;
    free(h);
    h=0;
  }
}
void freeDistributionContents(DistributionFunction *h) { 

  int i;
  if (h && h->word && h->number) {
    for (i=h->length-1;i>=0;i--) { 
      if (h->word[i]) 
	free(h->word[i]);
    }
    free(h->word);
    free(h->number);
    
    h->word = 0;
    h->number = 0;
    h->length=0;
    free(h);
    h=0;
  }
}

Mat* distanceMeasure(Mat *r, Mat *w, int n,
		     Mat *result,
		     DistanceFunctions *D, int k,
		     DistanceFunctionsPar *Dk, double *par, int kp ) { 


  
  
  int i,j;
  int N = FNumber+FParNumber;

  assert(result);
  if (debug) { 
    printf("distanceMeasure n =%d k=%d kp=%d\n",n,k,kp );
    printf("D %ld\n",(long int) D);
  }

  for (i=0;i<k;i++) {

    if(debug) { 
      printf("i = %d\n",i);
      printf("%s %ld\n",FN[i], (long int) D[i]);
    }
    result[i] = D[i](r,w,n);
    result[i+N] = 0;
    if(debug) { 
      printf("%d %f %f \n",i,result[i], result[i+N]);
    }
  }
  for (j=0;j<kp;j++,i++) { 
    if(debug) { 
      printf("j = %d\n",j);
      printf("%s p=%f\n",FPN[j],par[j]);
    }
    result[i] = Dk[j](r,w,n,par[j]);
    result[i+N] = 0;
    if(debug) { 
      printf("%d %f %f \n",i,result[i], result[i+N]);
    }
  }

  return result;

}  

void printResultDistance(double *result, double *pvalue,char **fn, int lfn, char **fpn, int lfpn) { 
  int i,j;

  printf(" Method  Value  P-Value  Parameter\n");

  for (i=0;i<lfn;i++) { 
    printf(" %30s %5.4f %1.5f \n",fn[i],result[i],pvalue[i]);
  }
  for (j=0;j<lfpn;j++,i++) { 
    printf(" %30s %5.4f %1.5f %1.2f \n",fpn[j],result[i],pvalue[i],PAR[j]);
  }
}

int  printResultDistanceVertical(double *result, double *pvalue,char **fn, int lfn, char **fpn, int lfpn, double ratio, double pvt) { 
  int i,j;
  float  l,r;
  int equiv = 0;

  for (i=0;i<lfn;i++) 
    if (pvalue[i]<=pvt) 
      l++;
    else 
      r++;
  for (j=0;j<lfpn;j++,i++)  
    if (pvalue[i]<=pvt) 
      l++;
    else 
      r++;
  if (l/(r+l)>=ratio) {
    printf(" Equivalent intervals \n" );
    equiv = 1;
  } 
  else   printf(" Different  intervals \n" );

  printf(" Method  Value  P-Value  Parameter\n");
  

  for (i=0;i<lfn;i++) { 
    printf(" %30s %5.4f %1.5f \n",fn[i],result[i],pvalue[i]);
  }
  for (j=0;j<lfpn;j++,i++) { 
    printf(" %30s %5.4f %1.5f %1.2f \n",fpn[j],result[i],pvalue[i],PAR[j]);
  }

  return equiv;

}

static inline double *filtering(double *res, double *output) { 


  return res;
}





double* distanceHistograms(Histogram *sh1,
			   Histogram *sh2) { 

  int N,i;
  Mat *res,*result;
  Mat *df1,*df2;
  unsigned long int AIB,AUB;
  double Ratio;


  Histogram *eh1, *eh2;
  Histogram *un, *in;
  res = (Mat *)calloc(DIM*(FNumber+FParNumber),sizeof(double));

  eh1 = (Histogram *) malloc(sizeof(Histogram)); 
  eh2 = (Histogram *) malloc(sizeof(Histogram)); 
  un = (Histogram *) malloc(sizeof(Histogram)); 
  in = (Histogram *) malloc(sizeof(Histogram)); 

  SORT_HISTOGRAM(sh1);
  SORT_HISTOGRAM(sh2);
  extendHistogram(sh1,sh2,eh1,eh2);
  if (debug) {
    printHistogram(sh1);
    printHistogram(sh2);
    printHistogram(eh1);
    printHistogram(eh2);
  }
  N = eh1->length;
  

  UnionIntersectionHistogram(sh1,sh2,un,in);
  AIB =0;
  for (i=0;i<in->length;i++) { 
    AIB+=in->number[i];
  }
  if (debug) { 
    printf(" AIB %lu \n",AIB);


  }
  AUB =0;
  for (i=0;i<un->length;i++) { 
    AUB+=un->number[i];
  }
  if (debug) { 
    printf(" AUB %lu \n",AUB);


  }
  FREE_EXTENDED_HIST(un);
  FREE_EXTENDED_HIST(in);
  
  
  

  Ratio = ((double)AIB)/((double)AUB);

  if (Ratio< RATIO_INTERSECTION_UNION_THRESHOLD) {
    int i;
    if (debug) { 
      printf("Ratio %f \n",Ratio);


    }

    for (i=0;i<DIM*(N_METHODS);i++) {
      res[i] = 1;
    }
  } else { 

    df1 =  DistributionFunctionFromHistogram(eh1->number,N,1);
    df2 =  DistributionFunctionFromHistogram(eh2->number,N,1);
    assert(df1 && df2); 
    if (debug) {
      printPDF_DFHistogram(df1,N);
      printPDF_DFHistogram(df2,N);
    }
    
    res = DFMeasure(df1,df2,N,res);
    //  free(res);
    free(df1);
    free(df2);
  }
    
    
    // printResultDistance(res,FN,FNumber,FPN,FParNumber);
  result = filtering(res,result);
    
  FREE_EXTENDED_HIST(eh1);
  FREE_EXTENDED_HIST(eh2);

  if (debug) { 
    printf("End  distanceHistograms\n");
    
  }
  return result;
}  




double *filtering_quorum(double *res, double *output) { 

  int max= N_METHODS;
  int count=0;
  int i;
  double min=1000;
  output = (double*) calloc(2,sizeof(double));
  



  for (i=0;i<max;i++) {
    if (res[i+max]>0.95)
      count ++;
    if (fabs(res[i])< min)
      min = fabs(res[i]);
  }
  
  if ((((double)count)/max)>=0.5)
    output[1] = 1.0;
  else 
    output[1] = 0.0;
  output[0] = min;
  
  return output;
}

double *filtering_quorum_t(double *res, double *output, double threshold) { 

  int max= N_METHODS;
  int count=0;
  int i;
  double min=1000;
  output = (double*) calloc(2,sizeof(double));
  



  for (i=0;i<max;i++) {
    if (res[i+max]>0.95)
      count ++;
    if (fabs(res[i])< min)
      min = fabs(res[i]);
  }
  
  if ((((double)count)/max)>=threshold)
    output[1] = 1.0;
  else 
    output[1] = 0.0;
  output[0] = min;
  
  return output;
}

double *filtering_quorum_separate(double *val, double *pval, double threshold) { 

  int max= N_METHODS;
  int count=0;
  int i;
  double min=1000;
  double *output = (double*) calloc(2,sizeof(double));
  



  for (i=0;i<max;i++) {
    if (pval[i]>0.95)
      count ++;
    if (fabs(val[i])< min)
      min = fabs(val[i]);
  }
  
  if ((((double)count)/max)>threshold)
    output[1] = 1.0;
  else 
    output[1] = 0.0;
  output[0] = min;
  
  return output;
}




double* distanceHistogramsQ(Histogram *sh1,
			   Histogram *sh2) { 

  int N,i;
  Mat *res,*result;
  Mat *df1,*df2;
  unsigned long int AIB,AUB;
  double Ratio;


  Histogram *eh1, *eh2;
  Histogram *un, *in;
  res    = (Mat *) calloc(DIM*(N_METHODS),sizeof(double));
  //  result = (Mat *) calloc(2,sizeof(double));
  

  eh1 = (Histogram *) malloc(sizeof(Histogram)); 
  eh2 = (Histogram *) malloc(sizeof(Histogram)); 
  un = (Histogram *) malloc(sizeof(Histogram)); 
  in = (Histogram *) malloc(sizeof(Histogram)); 



  SORT_HISTOGRAM(sh1);
  SORT_HISTOGRAM(sh2);
  extendHistogram(sh1,sh2,eh1,eh2);
  if (debug) {
    printf("distanceHistogramsQ %d \n",DIM*(N_METHODS));
    printHistogram(sh1);
    printHistogram(sh2);
    printHistogram(eh1);
    printHistogram(eh2);
  }
  N = eh1->length;
  

  UnionIntersectionHistogram(sh1,sh2,un,in);
  AIB =0;
  for (i=0;i<in->length;i++) { 
    AIB+=in->number[i];
  }
  if (debug) { 
    printf(" AIB %lu \n",AIB);


  }
  AUB =0;
  for (i=0;i<un->length;i++) { 
    AUB+=un->number[i];
  }
  if (debug) { 
    printf(" AUB %lu \n",AUB);


  }
  FREE_EXTENDED_HIST(un);
  FREE_EXTENDED_HIST(in);
  
  
  

  Ratio = ((double)AIB)/((double)AUB);

  if (Ratio< RATIO_INTERSECTION_UNION_THRESHOLD) {
    int i;
    if (debug) { 
      printf("Ratio %f \n",Ratio);


    }

    for (i=0;i<DIM*(N_METHODS);i++) {
      res[i] = 1;
    }
  } else { 

    df1 =  DistributionFunctionFromHistogram(eh1->number,N,1);
    df2 =  DistributionFunctionFromHistogram(eh2->number,N,1);
    assert(df1 && df2); 
    if (debug) {
      printPDF_DFHistogram(df1,N);
      printPDF_DFHistogram(df2,N);
    }
    
    if (debug) { 
      printf("DFMeasure begin\n");
    }
    res = DFMeasure(df1,df2,N,res);
    if (debug) { 
      printf("DFMeasure end\n");
    }

    
    //    free(res);
    free(df1);
    free(df2);
  }
    
    
    // printResultDistance(res,FN,FNumber,FPN,FParNumber);
  result = filtering_quorum(res,result);
  if (debug) { 
    printf("End  filtering quorum\n");
    
  }
  
  if (res) { free(res); res=0;}
  if (debug) { 
    printf("Free res\n");
    
  }
  FREE_EXTENDED_HIST(eh1);
  FREE_EXTENDED_HIST(eh2);

  if (debug) { 
    printf("End  distanceHistogramsQ\n");
    
  }
  return result;
}  
double* distanceHistogramsQQ(Histogram *sh1,
			     Histogram *sh2, 
			     double threshold) { 

  int N,i;
  Mat *res,*result;
  Mat *df1,*df2;
  unsigned long int AIB,AUB;
  double Ratio;


  Histogram *eh1, *eh2;
  Histogram *un, *in;
  res    = (Mat *) calloc(DIM*(N_METHODS),sizeof(double));
  //  result = (Mat *) calloc(2,sizeof(double));
  

  eh1 = (Histogram *) malloc(sizeof(Histogram)); 
  eh2 = (Histogram *) malloc(sizeof(Histogram)); 
  un = (Histogram *) malloc(sizeof(Histogram)); 
  in = (Histogram *) malloc(sizeof(Histogram)); 


  if (debug) {
    printf("distanceHistogramsQ previous sorting %d \n",DIM*(N_METHODS));
    printHistogram(sh1);
    printHistogram(sh2);
  }


  SORT_HISTOGRAM(sh1);
  SORT_HISTOGRAM(sh2);
  extendHistogram(sh1,sh2,eh1,eh2);
  if (debug) {
    printf("distanceHistogramsQ %d \n",DIM*(N_METHODS));
    printHistogram(sh1);
    printHistogram(sh2);
    printHistogram(eh1);
    printHistogram(eh2);
  }
  N = eh1->length;
  

  UnionIntersectionHistogram(sh1,sh2,un,in);
  AIB =0;
  for (i=0;i<in->length;i++) { 
    AIB+=in->number[i];
  }
  if (debug) { 
    printf(" AIB %lu \n",AIB);


  }
  AUB =0;
  for (i=0;i<un->length;i++) { 
    AUB+=un->number[i];
  }
  if (debug) { 
    printf(" AUB %lu \n",AUB);


  }
  FREE_EXTENDED_HIST(un);
  FREE_EXTENDED_HIST(in);
  
  
  

  Ratio = ((double)AIB)/((double)AUB);

  if (Ratio< RATIO_INTERSECTION_UNION_THRESHOLD) {
    int i;
    if (debug) { 
      printf("Ratio %f \n",Ratio);


    }

    for (i=0;i<DIM*(N_METHODS);i++) {
      res[i] = 1;
    }
  } else { 

    df1 =  DistributionFunctionFromHistogram(eh1->number,N,1);
    df2 =  DistributionFunctionFromHistogram(eh2->number,N,1);
    assert(df1 && df2); 
    if (debug) {
      printPDF_DFHistogram(df1,N);
      printPDF_DFHistogram(df2,N);
    }
    
    if (debug) { 
      printf("DFMeasure begin\n");
    }
    res = DFMeasure(df1,df2,N,res);
    if (debug) { 
      printf("DFMeasure end\n");
    }

    
    //    free(res);
    free(df1);
    free(df2);
  }
    
    
  //printResultDistance(res,FN,FNumber,FPN,FParNumber);
  //PRINTRESULTDISTANCE(res,res+N_METHODS);
  result = filtering_quorum_t(res,result,threshold);
  if (debug) { 
    printf("End  filtering quorum\n");
    
  }
  
  if (res) { free(res); res=0;}
  if (debug) { 
    printf("Free res\n");
    
  }
  FREE_EXTENDED_HIST(eh1);
  FREE_EXTENDED_HIST(eh2);

  if (debug) { 
    printf("End  distanceHistogramsQ\n");
    
  }
  return result;
}  

double* distanceHistogramsQPerl(
				char **x1, double *y1, int len1,
				char **x2, double *y2, int len2,
				double threshold ) {
  double *res; 
  Histogram h1,h2;
  h1.word = x1;
  h1.number = y1; 
  h1.length = len1;
  h2.word = x2; 
  h2.number =  y2; 
  h2.length = len2; 
  
  if (debug) {   
    printHistogram(&h1);
    printHistogram(&h2);
  } 
  res =  distanceHistogramsQQ(&h1,&h2, threshold);
  
  if (debug) {
    printf("Res d=%e p-val=%e \n",res[0],res[1]);
  }                                             
  return res;
} 
 
double* exposedDistanceHistogramsQPerl(char **x1, double *y1, int len1,   
				       char **x2, double *y2, int len2,
				       int *len ) {                    
  double *res;         
  Histogram h1,h2;  
  h1.word = x1;       
  h1.number = y1;    
  h1.length = len1;   
  h2.word = x2;        
  h2.number =  y2;     
  h2.length = len2;    
  
  *len = DIM*(N_METHODS);

  if (debug) {  
    printHistogram(&h1);
    printHistogram(&h2); 
  }
  res =  distanceHistograms(&h1,&h2);  
  if (debug) {
    printf("Res d=%e p-val=%e \n",res[0],res[1]); 
  }
  return res; 
} 
 
double *
extendedCompareHistogram(char **x1, double *y1, int len1,
				char **x2, double *y2, int len2,
				double threshold,
				double *distance,
				double *pvalues,
				int *outlen 
				) { 
  
  double pvalue;
  int i;
  int L = *outlen;
  double *res = exposedDistanceHistogramsQPerl(x1,y1, len1,
					       x2,y2, len2,
					       outlen);
  double *sum = filtering_quorum_t(res,sum,threshold);
  
  pvalue = sum[1];
  

  L = (L>(*outlen))?(*outlen):L;
  for (i=0;i<L;i++) { 
    
    distance[i] = res[i];
    pvalues[i] = res[i+(*outlen)];
  }

  free(res);
  
  return sum;
  
}


/************************
 * word is shared 
 * 
 */ 


DistributionFunction *DFFromHist(Histogram *h) { 

  
  DistributionFunction *r = (DistributionFunction *) calloc(1,sizeof(DistributionFunction));
  assert(r);
  r->number =  DistributionFunctionFromHistogram(h->number,
						 h->length,
						 1);
  r->word   = h->word;
  r->length = h->length;

  return r;

} 


DistributionFunction *SampleDistribution(DistributionFunction *h, int samples) { 


  if (!samples)  
    return 0;
  else if (samples >= h->length)  { 
    return h;
  }
  else {
    DistributionFunction *r =(DistributionFunction *) calloc(1,sizeof(DistributionFunction));
    int P = (h->length)/samples;
    int i;
    
    assert(r);
    r->length = h->length/P - (((h->length)%P)?1:0);
    
    r->word   = (char **) calloc(r->length,sizeof(char *));
    assert(r->word);
    r->number = (Mat *) calloc(r->length,sizeof(Mat));
    assert(r->number);
    
    for (i=0;i<r->length;i++) { 
      r->number[i] = h->number[i*P];
    }
    for (i=0;i<r->length;i++) {
      r->word[i] = (char *) malloc(sizeof(char)*(1+strlen(h->word[i*P])));
      assert(r->word[i]);
      strcpy(r->word[i],h->word[i*P]);
    }
    
    
    return r;
  }
  
} 


LinkedListAdjacentMatrix* allToAllHistogramComparison(Histogram *docs,int max, 
						      LinkedListAdjacentMatrix* adj,
						      double threshold) { 
  
  int i,j;
  double *res;
  LinkedListAdjacentMatrix *temp;
  
  for (i=max-2;i>=0; i--) { 
    for (j=max-1; j>i; j--) { 
      res = distanceHistogramsQQ(docs+i,docs +j, threshold);
      if (res[1]< 0.95) {
	temp = (LinkedListAdjacentMatrix*) malloc(sizeof(LinkedListAdjacentMatrix));
	temp -> next = adj;
	temp -> res = res;
	temp -> i = i;
	temp -> j = j;
	adj = temp;
      }
      else { 
	free(res);
      }
    }
    
  }


  
  return adj;
}

LinkedListAdjacentMatrix* 
allToAllHistogramArrayComparison(char **bins, double *values, int *lengths, int max, 
				 double threshold,
				 LinkedListAdjacentMatrix* adj) { 
  
  int i,j;
  double *re;
  Histogram *docs = (Histogram*) malloc(max*sizeof(Histogram));
  
  for (i=0; i<max;i++) { 
    docs[i].length = lengths[i];
    docs[i].word = bins;
    docs[i]. number = values;
    bins   += lengths[i];
    values += lengths[i];
  }
  adj = allToAllHistogramComparison(docs,max,adj,threshold);
  
  free(docs);
				    
  return adj;
}


int  free_adj_mat_cont( LinkedListAdjacentMatrix* adj) {
   LinkedListAdjacentMatrix* temp;

  if (!adj) return 0;
  
  while (adj->next) {
    temp = adj;
    if (temp ->res) { free(temp->res);  temp->res = 0; }
    adj = adj->next;
    free(temp);
  }

  return 1;
}


// labels sorted in increasing value 

double* distanceHistogramsQQNumericLabel(HistogramNumericLabel *sh1,
					 HistogramNumericLabel *sh2, 
					 double threshold) { 

  int N,i;
  Mat *res,*result;
  Mat *df1,*df2;
  unsigned long int AIB,AUB;
  double Ratio;


  HistogramNumericLabel *eh1, *eh2;
  res    = (Mat *) calloc(DIM*(N_METHODS),sizeof(double));
  //  result = (Mat *) calloc(2,sizeof(double));
  

  eh1 = (HistogramNumericLabel *) malloc(sizeof(HistogramNumericLabel)); 
  eh2 = (HistogramNumericLabel *) malloc(sizeof(HistogramNumericLabel)); 



  mergeExtendHistogramNumericLabel(sh1,sh2,eh1,eh2,1);
  N = eh1->length;
  
  
  
  df1 =  DistributionFunctionFromHistogram(eh1->number,N,1);
  df2 =  DistributionFunctionFromHistogram(eh2->number,N,1);
  assert(df1 && df2); 
  if (debug) {
    printPDF_DFHistogram(df1,N);
    printPDF_DFHistogram(df2,N);
  }
  
  if (debug) { 
    printf("DFMeasure begin\n");
  }
  res = DFMeasure(df1,df2,N,res);
  if (debug) { 
    printf("DFMeasure end\n");
  }
  
  
  //    free(res);
  free(df1);
  free(df2);

    
    
  //printResultDistance(res,FN,FNumber,FPN,FParNumber);
  //PRINTRESULTDISTANCE(res,res+N_METHODS);
  result = filtering_quorum_t(res,result,threshold);
  if (debug) { 
    printf("End  filtering quorum\n");
    
  }
  
  if (res) { free(res); res=0;}
  if (debug) { 
    printf("Free res\n");
    
  }
  FREE_EXTENDED_HIST(eh1);
  FREE_EXTENDED_HIST(eh2);

  if (debug) { 
    printf("End  distanceHistogramsQ\n");
    
  }
  return result;
}  



double* distanceHistogramsQPerlNumericLabel(double *x1, double *y1, int len1,                                                                                                                                                                  
					    double *x2, double *y2, int len2,                                                                                                                                                                  
					    double threshold) { 



  
  double *res; 
  HistogramNumericLabel h1,h2;
  h1.word = x1;
  h1.number = y1; 
  h1.length = len1;
  h2.word = x2; 
  h2.number =  y2; 
  h2.length = len2; 
  

  res =  distanceHistogramsQQNumericLabel(&h1,&h2, threshold);
  
  if (debug) {
    printf("Res d=%e p-val=%e \n",res[0],res[1]);
  }                                             
  return res;




}                                                                                                                                                                                


