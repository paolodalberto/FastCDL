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
#ifndef MOVINGAVERAGEINTERFACE
#define MOVINGAVERAGEINTERFACE




struct stateMA { 
  unsigned int sizeinbytes;
  unsigned int code;
  Mat o_t; // o_t = 1/n Sum_{j=t-N}^y x_j
  Mat e_t; // e_t = 1/n Sum_{j=t-N}^y (o_j-x_j)^2
  Mat gamma;
  CircularBuffer *x; // x_{t-N} ... x_{t}
  CircularBuffer *e; // o_{t-N} ... x_{t} 
  CircularBuffer *t; // time_{t-N} ... time_{t} 
};

typedef struct stateMA MovingAverageState;

#define ALLOCATE_MA_STATE(f,N,g) {					\
    (f) = (HoltWinterState *) malloc(sizeof(HoltWinterState));		\
    assert(f);								\
    (f)->sizeinbytes = sizeof(HoltWinterState);				\
    if (N>0) { ALLOCATECIRCULAR((f)->x,N);} else (f)->x=0;		\
    if (N>0) { ALLOCATECIRCULAR((f)->e,N);} else (f)->e=0;		\
    if (N>0) { ALLOCATECIRCULAR((f)->t,N);} else (f)->t=0;		\
    (f)->gamma=g;							\
    if (N>0) {                                                          \
      (f)->sizeinbytes += (f)->sizeinbytes%sizeof(CircularBuffer);	\
      (f)->sizeinbytes += (f)->x->sizeinbytes;			        \
      (f)->sizeinbytes += (f)->sizeinbytes%sizeof(CircularBuffer);	\
      (f)->sizeinbytes += (f)->e->sizeinbytes;				\
      (f)->sizeinbytes += (f)->sizeinbytes%sizeof(CircularBuffer);	\
      (f)->sizeinbytes += (f)->t->sizeinbytes;				\
    }									\
  } 



#define FREE_MA_STATE(f) {						\
    if (f) {								\
      if (f->x) {							\
        FREECIRCULAR((f)->x);						\
        FREECIRCULAR((f)->e);						\
        FREECIRCULAR((f)->t);						\
      }									\
      free(f); (f)=0;							\
    }									\
  } 


#define PRINTMASTATE(t,i)         free(PrintMAState(t,i,1))


#ifndef MOVINGAVERAGE_MODULE

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  extern int MovingAverage(TimeSeries *stream,
			   MovingAverageState **state,
			   TimeSeries **ytmOut,
			   TimeSeries **dtmOut,
			   Mat gamma
			   );
  
  // these are for serialization and printing 
  extern char * PrintMAState(MovingAverageState *t, int identation, int P);
  extern MovingAverageState * ReadMAState(int identation, char **temp);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif





#endif
