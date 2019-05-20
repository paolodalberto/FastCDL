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
#ifndef MARTINGALEPDF
#define MARTINGALEPDF 1

//#define MINIMUM_DISTANCE 1
#define AVERAGE_DISTANCE 1


#include<window.h>
#include<martingale.h>

#ifndef MARTINGALEPDF_MODULE

#if defined(c_plusplus) || defined(__cplusplus) 
extern "C" {
#endif
  extern double pdfbased_distance_pvalue(TimeSeries *ts, SinglePoint *sp, void *state);
  extern int binary_search_array(double *array, double  sp, int stat_len);
  extern Adj* pdf_based_on_distance(TimeSeries *ts, unsigned int N, Adj *s);
#if defined(c_plusplus) || defined(__cplusplus) 
}
#endif 

#endif //#ifndef MARTINGALEPDF_MODULE


#endif
