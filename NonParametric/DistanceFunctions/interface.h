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
#ifndef INTERFACESTOCHASTICDISTANCE
#define INTERFACESTOCHASTICDISTANCE 1

#include <type.h>

#define RATIO_INTERSECTION_UNION_THRESHOLD 0.3



#define DIM 2


struct hist { 
  
  char **word;
  Mat  *number;
  int length;
};

struct numerichist { 
  
  Mat  *word;
  Mat  *number;
  int length;
};




struct df { 
  
  char **word;
  Mat  *number;
  int length;
};

struct singleton { 
  
  char *word;
  Mat  number;
};



typedef struct adjMatrix LinkedListAdjacentMatrix;

struct adjMatrix { 
  int i,j;
  Mat *res;
  LinkedListAdjacentMatrix *next;
};


typedef struct numerichist HistogramNumericLabel;  
typedef struct hist Histogram;  
typedef struct singleton  Singleton;  
typedef struct df DistributionFunction;  




#define FREE_EXTENDED_HIST(h) {				\
    if (h) {						\
      if (h->number) { free(h->number); h->number=0; }	\
      if (h->word)   { free(h->word);   h->word=0; }	\
      free(h); h=0;}					\
  }

#define FREE_DF(h) {					\
    if (h) {						\
      if (h->number) { free(h->number); h->number=0; }	\
      free(h); h=0;}					\
  }




#ifndef INTERFACE_MODULE 
#include <distance.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  extern double *filtering_quorum_t(double *res, double *output, double threshold);
  extern double *filtering_quorum(double *res, double *output);
  extern double *filtering_quorum_separate(double *val, double *pval, double threshold);
  extern void printPDF_DFHistogram(Mat *h, int N);
  extern void printHistogram(Histogram *h);
  extern void printDF(DistributionFunction *h);
  extern double* distanceHistograms(Histogram *sh1,
				    Histogram *sh2);
  extern double* distanceHistogramsQ(Histogram *sh1,
				    Histogram *sh2);
  
  extern double* distanceHistogramsQQ(Histogram *sh1,
				      Histogram *sh2, 
				      double threshold);

  extern double* distanceHistogramsQQNumericaLabel(
						   HistogramNumericLabel *sh1,
						   HistogramNumericLabel *sh2, 
						   double threshold);

  extern Mat* distanceMeasure(Mat *r, Mat *w, int n, 
			      Mat *result,
			      DistanceFunctions *D, int k,
			      DistanceFunctionsPar *Dk, double *par, int kp );
  
  extern void printResultDistance(double *res,double *pvalue, char **fn, int lfn, char **fpn, int lfpn); 
  extern int  printResultDistanceVertical(double *res,double *pvalue, char **fn, int lfn, char **fpn, int lfpn,double ratio, double ptv); 
  extern void freeHistogramContents(Histogram *h);
  extern void freeDistributionContents(DistributionFunction *h);
  extern DistributionFunction *DFFromHist(Histogram *h);
  extern DistributionFunction *SampleDistribution(DistributionFunction *h, int samples); 
  extern double* distanceHistogramsQPerl(char **x1, double *y1, int len1,                                                                                                                                                                  
                                         char **x2, double *y2, int len2,                                                                                                                                                                  
                                         double threshold);                                                                                                                                                                                
  extern double* distanceHistogramsQPerlNumericLabel(double *x1, double *y1, int len1,                                                                                                                                                                  
						     double *x2, double *y2, int len2,                                                                                                                                                                  
						     double threshold);                                                                                                                                                                                

  extern double *exposedDistanceHistogramsQPerl(char **x1, double *y1, int len1,                                                                                                                                                                  
						char **x2, double *y2, int len2,                                                                                                                                                                  
						int *len);                                                                                                                                                                                
  
  extern double *extendedCompareHistogram(char **x1, double *y1, int len1,
					  char **x2, double *y2, int len2,
					  double threshold,
					  double *distance,double *pvalues,
					  int *outlen 
					  );

  extern LinkedListAdjacentMatrix* allToAllHistogramComparison(Histogram *doc,
							       int max, 
							       LinkedListAdjacentMatrix* adj);

  extern LinkedListAdjacentMatrix* allToAllHistogramArrayComparison(char **bins, double *values, int *lenghts, int max, 
								    LinkedListAdjacentMatrix* adj);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif




#endif




