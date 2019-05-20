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
#ifndef STRANGENESSFUNCTIONS
#define STRANGENESSFUNCTIONS

#include<martingale.h>
#include<assert.h>
#include<window.h>
#include<sort.h>
#include<quicksort.h>


#define NNMartingale(ts,state,val,pval,ym,m) stochastic_martingale(ts,state,(val),(pval),(ym),0.92,3,20,m,min_cluster_distance_pvalue)


#ifndef STRANGENESS_MODULE

#if defined(c_plusplus) || defined(__cplusplus) 
extern "C" {
#endif

  extern double min_cluster_distance_pvalue(TimeSeries *ts, SinglePoint *sp, void *state);
  extern double * min_cluster_alpha_i(TimeSeries *ts);

#if defined(c_plusplus) || defined(__cplusplus) 
}
#endif 

#endif //#ifndef STRANGENESS _MODULE


#endif
