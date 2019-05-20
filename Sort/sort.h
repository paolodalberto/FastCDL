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
#ifndef __SORT_H__
#define __SORT_H__



#include <math.h>

#include <float.h>


#ifndef MIN 
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif




#define GTX(x, y) ((x) > (y))
#define LTX(x, y) ((x) < (y))
#define GEX(x, y) ((x) >= (y))
#define LEX(x, y) ((x) <= (y))
#define EQX(x, y) ((x) == (y))
#define NEX(x, y) ((x) != (y))





#define GTW(x, y) (strcmp((x),(y))>0)
#define LTW(x, y) (strcmp((x),(y))<0)
#define GEW(x, y) (strcmp((x),(y))>=0)
#define LEW(x, y) (strcmp((x),(y))<=0)
#define EQW(x, y) (strcmp((x),(y))==0)
#define NEW(x, y) (strcmp((x),(y))!=0)

#define GTT(left, i, right, j, key) ((key)?(GT(left->data[i],right->data[j])):(GTX(left->data[i].x,right->data[j].x)))
#define LTT(left, i, right, j, key) ((key)?(LT(left->data[i],right->data[j])):(LTX(left->data[i].x,right->data[j].x)))
#define GET(left, i, right, j, key) ((key)?(GE(left->data[i],right->data[j])):(GEX(left->data[i].x,right->data[j].x)))
#define LET(left, i, right, j, key) ((key)?(LE(left->data[i],right->data[j])):(LEX(left->data[i].x,right->data[j].x)))
#define EQT(left, i, right, j, key) ((key)?(EQ(left->data[i],right->data[j])):(EQX(left->data[i].x,right->data[j].x)))
#define NET(left, i, right, j, key) ((key)?(NE(left->data[i],right->data[j])):(NEX(left->data[i].x,right->data[j].x)))



#define SWAP(x, y)    {				\
    Mat temp;					\
    temp = (x);					\
    (x) = (y);					\
    (y) = temp;					\
  }
#define SWAPH(x, i,j) {							\
    Mat   tn;								\
    char* ts;								\
    tn = x->number[i];							\
    ts= x->word[i];							\
    x->number[i] = x->number[j];					\
    x->word[i] = x->word[j];						\
    x->number[j] = tn;							\
    x->word[j] = ts;							\
  }
#define SWAPT(a, i,j) {							\
    SinglePoint   tn;								\
    tn = a->data[i];							\
    a->data[i] = a->data[j];							\
    a->data[j] = tn;							\
  }


#endif


