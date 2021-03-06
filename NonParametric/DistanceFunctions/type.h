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
#ifndef TYPE_SPECIFICATION
#define TYPE_SPECIFICATION 1


typedef double Mat;
typedef long int Xorder;


#if (x86_64)
typedef unsigned long int ADDRESS;
#elif (i386)
typedef unsigned int  ADDRESS;
#else
typedef unsigned int ADDRESS ;
#endif

#ifdef GETTIME
#include <sys/time.h>
struct timeval _t1,_t2;
double _duration;

#define START_CLOCK   gettimeofday(&_t1,NULL ); 
#define END_CLOCK   gettimeofday(&_t2,NULL);   _duration = (_t2.tv_sec-_t1.tv_sec)+ (double)(_t2.tv_usec-_t1.tv_usec)/1000000;    fprintf(stderr,"----------> get time %e sec<------\n",_duration); 
#endif /*  GETTIME */


#ifdef CLOCK
#include <time.h>
clock_t _t1,_t2;
double _duration;

#define START_CLOCK   _t1 = clock(); 
#define END_CLOCK     _t2 = clock(); _duration =  ((double)(_t2-_t1))/CLOCKS_PER_SEC; \
  fprintf(stderr,"clock time %e s \n", _duration); 
#endif

#define TEMP temp,  cM, cN, cM, cN
#define INTERVAL (45)   /* two minutes */


/************************************************
 * This is a timing procedure very common in CMU
 * Notice that we execute X once before we start timing.
 *
 */

#define TIMING(X,time, interval) { int i,j;\
j=1;\
X;\
do {\
  j*=2;\
  START_CLOCK;\
  for (i=0;i<j;i++) { X;  }\
  END_CLOCK;\
 } while (_duration<interval);\
 time = (double)_duration/(double)j;		\
 printf("times %d average time %f\n",j,time);\
}



//#define PVALUE_MODE_0    1
//#define SANTANU_HISTOGRAM 1
//#define REGULAR_MODE 1 

#if(PVALUE_MODE_0)
#define N_METHODS (FNumber+FParNumber)
#define DFMeasure(df1,df2,N,res) distanceMeasure(df1,df2,N,res,F,FNumber,FP,PAR,FParNumber)
#define PRINTRESULTDISTANCE(res,pvalue) printResultDistance(res,pvalue,FN,FNumber,FPN,FParNumber);
#define PRINTRESULTDISTANCEVERTICAL(res,pvalue,ratio,ptv) printResultDistanceVertical(res,pvalue,FN,FNumber,FPN,FParNumber,ratio,ptv);
#elif(SANTANU_HISTOGRAM)
#define N_METHODS (SANTANU_FNumber+SANTANU_FParNumber)
#define DFMeasure(df1,df2,N,res) distanceMeasurePValue(df1,df2,N,res,SANTANU_F,SANTANU_FNumber,SANTANU_FP,SANTANU_PAR,SANTANU_FParNumber)
#define PRINTRESULTDISTANCE(res,pv)                       printResultDistance(res,pv,SANTANU_FN,SANTANU_FNumber,SANTANU_FPN,SANTANU_FParNumber);
#define PRINTRESULTDISTANCEVERTICAL(res,pvalue,ratio,ptv) printResultDistanceVertical(res,pvalue,SANTANU_FN,SANTANU_FNumber,SANTANU_FPN,SANTANU_FParNumber,ratio,ptv);
#elif(REGULAR_MODE)
#define N_METHODS (ANOMALY_FNumber+ANOMALY_FParNumber)
#define DFMeasure(df1,df2,N,res) distanceMeasurePValue(df1,df2,N,res,ANOMALY_F,ANOMALY_FNumber,ANOMALY_FP,ANOMALY_PAR,ANOMALY_FParNumber)
#define PRINTRESULTDISTANCE(res,pv)                      printResultDistance(res,pv,ANOMALY_FN,ANOMALY_FNumber,ANOMALY_FPN,ANOMALY_FParNumber);
#define PRINTRESULTDISTANCEVERTICAL(res,pv,ratio,ptv)   printResultDistanceVertical(res,pv,ANOMALY_FN,ANOMALY_FNumber,ANOMALY_FPN,ANOMALY_FParNumber,ratio,ptv);
#else
#define N_METHODS (FNumber+FParNumber)
#define DFMeasure(df1,df2,N,res) distanceMeasurePValue(df1,df2,N,res,FPV,FNumber,FPPV,PAR,FParNumber)
#define PRINTRESULTDISTANCE(res,pvalue) printResultDistance(res,pvalue,FN,FNumber,FPN,FParNumber);
#define PRINTRESULTDISTANCEVERTICAL(res,pvalue,ratio,ptv) printResultDistanceVertical(res,pvalue,FN,FNumber,FPN,FParNumber,0.5,0.98);
#endif



#endif
