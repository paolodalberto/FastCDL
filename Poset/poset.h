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
#ifndef POSETH
#define POSETH

typedef struct order PartialOrder;
typedef struct chain_list  Chain_Heads;

#include<stdio.h>
#include<window.h>
#include<sort.h>
#include<quicksort.h>
#include<assert.h>
#include<string.h>

//enum TopologicalOrderType { MST, POSET } ;

#define MST   0
#define POSET 1 





/************
 * Poset DAG structure
 * MAX -> MIN and all the ones in between
 */ 



struct order { 
  int out;    // # outlinks
  int in;     // # inlinks
  int visit;  // for DF construction

  int print;   // for printing
  unsigned int code;   // for construction/destruction 
  unsigned int count;  // for DF computation
  unsigned int id;
  unsigned int td;

  SinglePoint **t;
  int common_points;

  PartialOrder **outlinks;
  PartialOrder **inlinks ;

  PartialOrder **relations;
  int *nonEmptyRelations;
  int numberOfRelation;

};





#define ALLOCATE_POSET(p,MAX) {                                          \
    p = (PartialOrder *) calloc(1,sizeof(PartialOrder));                 \
    p->code = 1; p->count = 1; p->print=1;                               \
    /*p->t  = (SinglePoint **) calloc(MAX/2+1,sizeof(SinglePoint *));    */ \
    p->t  = (SinglePoint **) calloc(MAX,sizeof(SinglePoint *));     \
    p->common_points = 0;                                                \
    p->outlinks = (PartialOrder **) calloc(3*(MAX+2),sizeof(PartialOrder*)); \
    assert(p->outlinks);                                                 \
    p->inlinks   = p->outlinks + MAX;                                     \
    p->relations = p->inlinks  + MAX; \
    p->nonEmptyRelations = (int*) calloc(MAX,sizeof(int)); \
    assert( p->nonEmptyRelations);\
}

#define ALLOCATE_POSET_T(p,top,MAX) {                                    \
    p = (PartialOrder *) calloc(1,sizeof(PartialOrder));                 \
    p->code = 1; p->count = top->code;                       \
    p->t  = (SinglePoint **) calloc(1,sizeof(SinglePoint *));  p->t[0]=top \
    p->common_points = 0;                                                \
    p->outlinks = (PartialOrder **) calloc(3*MAX,sizeof(PartialOrder*)); \
    assert(p->outlinks);                                                 \
    p->inlinks   = p->outlinks + MAX;                                     \
    p->relations = p->inlinks  + MAX; \
    p->nonEmptyRelations = (int*) calloc(MAX,sizeof(int)); \
    assert( p->nonEmptyRelations);\
}



/*
#define ADD_OUTLINK(p,o) {         \
   (p)->outlinks[(p)->out]= (o);((p)->out)++;      \
   (o)->inlinks [(o)->in] = (p); ((o)->in)++;\
}
*/


/************
 * Independent chains
 *
 */ 




struct chain_list { // chain of in-comparable elements.
  unsigned int length;
  PartialOrder **chain;  // ordered array
  Chain_Heads  *smaller; // next  
};


#define ALLOCATE_CHAINS(c,e,max) (c)=allocate_chains((e),(max))


// a chain will be parallel to the other and it will follow the same strong order
#define INSERT_CHAIN(c,e) { \
 e->smaller = c->smaller;            \
 c->smaller = e;                     \
}
 

// this is done in combination with combine so I will never clean the list.
#define REMOVE_DOMINATED_CHAIN(c, e, prev) {                  \
  if (e->smaller) { prev->smaller = e->smaller;  }            \
  else            { prev->smaller =0; }                       \
  if (e->chain) { free(e->chain); e->chain=0; e->length =0;}  \
  free(e); e->smaller=0;                                      \
  }






  /*
#define COMBINE_DOMINATED_CHAIN(c,e,prev,d) {                                  \
  memcpy(d->chain+d->length,e->chain,e->length*sizeof(PartialOrder *));        \
  d->length += e->length;                                                      \
  REMOVE_DOMINATED_CHAIN(c,e, prev);                                           \
  }
  */


#define DOMINATES_PO(d,e) (GE((d),(e))==MIN((e).dimensions,(d).dimensions)) 
#define DOMINATES_CHAINS(d,e) (DOMINATES_PO(*((d)->t[0]),*((e)->chain[0]->t[0]))) 
 




struct posetqueue { 
  int top,bottom; 
  unsigned int length;

  PartialOrder **array;
};

typedef struct posetqueue PQueue;

#define IN(f,x) { (f)->array[f->bottom] = x; f->bottom = (((f)->bottom+1)%((f)->length)); }
#define OUT(f,x)  { (x) = (f)->array[(f)->top]; (f)->top = (((f)->top+1)%((f)->length)); }
#define EMPTY(f)  ((f)->bottom == (f)->top) 

#define PRINTQUEU(f) { 

#define ALLOCATECIRCULARQUEUE(f,N) { \
    (f)= (PQueue *) calloc(1,sizeof(PQueue));		\
    assert(f);								\
    (f)->length = N;							\
    (f)->array = (PartialOrder **) calloc(N,sizeof(PartialOrder*)); \
    assert((f)->array);  \
   }


#define FREECIRCULARQUEUE(f) {					\
    if (f) {							\
      if ((f)->array) {                                         \
         free((f)->array);                                 \
      } free(f); (f) = 0;					\
    }								\
  }


#define MAXCODE 2

#include<minimumspanningtree.h>





  
#ifndef POSET_MODULE
  
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  
  extern Window       *createExtendedStatistics(TimeSeries *ref, TimeSeries *win , int  type_order);
  extern Window       *createStatistics(TimeSeries *ts,int type_order); 
  extern void          FREE_POSET(PartialOrder **p, int len); 
  extern PartialOrder **partialOrder(TimeSeries *sortedTS, int *len);
  extern Chain_Heads *allocate_chains(PartialOrder *e, unsigned int max);
  extern Chain_Heads  *COMBINE_DOMINATED_CHAIN(Chain_Heads *e, PartialOrder *d);
  extern SinglePoint *findNearest(PartialOrder *p, unsigned int code,int max);
  extern SinglePoint *findNearest2(PartialOrder *p, unsigned int code, int max);
  extern void FREE_CHAIN( Chain_Heads *p);
  extern void ADD_OUTLINK(PartialOrder *p, PartialOrder *o);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif // POSET_MODULE




#endif
