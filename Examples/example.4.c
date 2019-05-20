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
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <malloc/malloc.h>

typedef unsigned int Xorder;
typedef double Mat;

 
struct ts { 
  unsigned int sizeinbytes;
  unsigned int max;
  unsigned int length;
  Xorder *x;
  Mat    *y;
};

typedef struct ts TimeSeries; 

#define ALLOCATETS(f,N) {						\
    (f)= (TimeSeries *) calloc(1,sizeof(TimeSeries));			\
    assert(f);								\
    (f)->sizeinbytes = (unsigned int)  sizeof(TimeSeries) ;		\
    (f)->sizeinbytes += ((f)->sizeinbytes)% sizeof(Xorder) ;		\
    printf("%d\n",(f)->sizeinbytes);					\
    (f)->length = 0; (f)->max =N;					\
    (f)->x = (Xorder *) calloc(N,sizeof(Xorder));			\
    assert(f->x);							\
    (f)->sizeinbytes += (unsigned int) N*sizeof(Xorder);		\
    (f)->sizeinbytes += ((f)->sizeinbytes)% sizeof(Mat) ;		\
    printf("%u\n",(f)->sizeinbytes);					\
    (f)->y = (Mat *) calloc(N,sizeof(Mat));				\
    assert((f)->y);							\
    (f)->sizeinbytes += (unsigned int) N*sizeof(Mat);			\
    printf("%u\n",(f)->sizeinbytes);					\
  }
#define FREETS(f) {				\
    if (f) {					\
      if ((f)->x) { free((f)->x); (f)->x = 0;}	\
      if ((f)->y) {free((f)->y); (f)->y=0; }	\
      free(f); (f)=0;				\
    }						\
  }


int main() {

  int N;
  TimeSeries *t;

  printf("sizeof Timeseries %d\n",sizeof(TimeSeries));
  printf("sizeof Xorder %d\n",sizeof(Xorder));
  printf("sizeof Mat %d\n",sizeof(Mat));
  printf("sizeof char %d\n",sizeof(char));

  printf("N ? \n");
  scanf("%d",&N);

  ALLOCATETS(t,N);

  printf("size in blocks %u \n",t->sizeinbytes);

  return 1;
}
