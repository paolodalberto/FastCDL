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
#ifndef PVALUE_H
#define PVALUE_H

#define SANTANU_FNumber 5
#define SANTANU_FParNumber 0

#define ANOMALY_FNumber 10
#define ANOMALY_FParNumber 0


#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <doubly_compensated_sumc.h>
#include <assert.h>
#include <distance.h>
#include <interface.h>
#include <sort.h>
#include <quicksort.h>
#include <type.h>

#define PHIOFA 0
#define KSIOFA 1
#define KOLMOGOROVSMIRNOV 2 
#define KOLMOGOROVSMIRNOVR 3
#define KULLBACKLEIBERI    4 
#define KULLBACKLEIBERJ    5
#define JINK               6
#define JINL               7
#define JENSENSHANNONDIVERGENCE  8
#define SIMPLIFIEDWILCOX         9
#define SIMPLIFIEDTTEST          10
#define SIMPLIFIEDCHISQUARE     11
#define CHISQUARE               12
#define VARIATIONALDISTANCE     13
#define HELLINGER        14
#define BHATTACHARYYA 15
#define GENERALIZEDKR 16
#define GENERALIZEDKS 17
#define GENERALIZEDK2S 18
#define CRAMERVONMISES 19
#define EUCLID         20
#define MINKOWSKY      21
#define CAMBERRA       22              
#define TOTAL_DISTANCE_FUNCTIONS 23

#define COL 7
#define PBRAKETS { 0.95, 0.96, 0.97, 0.98, 0.99, 0.999, 0.9999 }




struct randp { 

  double val;
  double pval;


};


typedef struct randp Statistic;


typedef Mat (*Nomalization)(unsigned int ); 

typedef Statistic (*DistanceFunctionsPvalue)(Mat *, Mat *, int  );
typedef Statistic (*DistanceFunctionsPvaluePar)(Mat *, Mat *, int, double);



#ifndef PVALUE_MODULE
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  extern Nomalization N[(CAMBERRA+1)];
  extern Mat TABLE_PVALUE[(CAMBERRA+1)*COL];


  extern DistanceFunctionsPvalue FPV[FNumber];
  extern DistanceFunctionsPvaluePar FPPV[FParNumber];

  extern double *SANTANU_PAR;
  extern DistanceFunctionsPvalue SANTANU_F[SANTANU_FNumber];
  extern DistanceFunctionsPvaluePar SANTANU_FP[SANTANU_FParNumber];
  extern char *SANTANU_FN[SANTANU_FNumber];
  extern char *SANTANU_FPN[SANTANU_FParNumber];

  extern double *ANOMALY_PAR;
  extern DistanceFunctionsPvalue ANOMALY_F[ANOMALY_FNumber];
  extern DistanceFunctionsPvaluePar ANOMALY_FP[ANOMALY_FParNumber];
  extern char *ANOMALY_FN[ANOMALY_FNumber];
  extern char *ANOMALY_FPN[ANOMALY_FParNumber];

  extern Statistic phiofAWithPValue(Mat *r, Mat *w, int n);
  extern Statistic KsiofAWithPValue(Mat *r, Mat *w, int n);
  extern Statistic KolmogorovSmirnovWithPValue(  Mat *r, Mat *w, int n);
  extern Statistic KullbackLeiberIWithPValue(Mat *r, Mat *w, int n);
  extern Statistic KullbackLeiberIWithPValue(Mat *r, Mat *w, int n);
  extern Statistic JinKWithPValue(Mat *r, Mat *w, int n);
  extern Statistic JinLWithPValue(Mat *r, Mat *w, int n);
  extern Statistic JensenShannonDivergenceWithPValue(Mat *r, Mat *w, int n);
  extern Statistic ChiSquareWithPValue(Mat *r, Mat *w, int n);
  extern Statistic HellingerWithPValue(Mat *r, Mat *w, int n);
  extern Statistic BhattacharyyaWithPValue(Mat *r, Mat *w, int n);
  extern Statistic GeneralizedKsWithPValue(Mat *r, Mat *w, int n, double s);
  extern Statistic GeneralizedKrWithPValue(Mat *r, Mat *w, int n, double s);
  extern Statistic GeneralizedK2sWithPValue(Mat *r, Mat *w, int n, double s);
  extern Statistic CramerVonMisesWithPValue(Mat *r, Mat *w, int n);
  extern Statistic EuclideWithPValue(Mat *r, Mat *w, int n);
  extern Statistic MinkowskyWithPValue(Mat *r, Mat *w, int n);
  extern Statistic CamberraWithPValue(Mat *r, Mat *w, int n); 
  extern double* distanceMeasurePValue(Mat *r, Mat *w, int n,
				       Mat *result,
				       DistanceFunctionsPvalue *D, int k,
				       DistanceFunctionsPvaluePar *Dk, double *par, int kp );
  extern double* distanceHistogramsPValue(Histogram *sh1,
					  Histogram *sh2);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
  
#endif
#endif
