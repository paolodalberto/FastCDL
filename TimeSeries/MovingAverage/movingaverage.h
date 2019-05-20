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
  SinglePoint *o_t; // o_t = 1/n Sum_{j=t-N}^y x_j
  SinglePoint *e_t; // e_t = 1/n Sum_{j=t-N}^y |o_j-x_j|
  Mat gamma;
  CircularBuffer *x; // x_{t-N} ... x_{t}
  CircularBuffer *e; // o_{t-N} ... x_{t} 
};

typedef struct stateMA MovingAverageState;

#define ALLOCATE_MA_STATE(f,N,k) {					\
    (f) = (MovingAverageState *) calloc(1,sizeof(MovingAverageState));	\
    assert(f);		    					        \
    ALLOCATE_SP((f)->o_t,k); ALLOCATE_SP((f)->e_t,k);                       \
    if (N>0) { ALLOCATECIRCULAR((f)->x,N,k);} else (f)->x=0;		\
    if (N>0) { ALLOCATECIRCULAR((f)->e,N,k);} else (f)->e=0;		\
  } 



#define FREE_MA_STATE(f) {						\
    if (f) {								\
      if (f->x) {							\
        FREECIRCULAR((f)->x);						\
        FREECIRCULAR((f)->e);						\
      }									\
      FREE_SP((f)->o_t); FREE_SP((f)->e_t);                               \
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
			   int P,
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
