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
#ifndef MSTH 
#define MSTH

#include<window.h>
//#include<poset.h>


typedef struct node Node;
typedef struct edge Edge;
typedef struct edges Edges;
typedef struct tree Tree;
typedef struct forest Forest;

struct node { 
  
  Tree *tree;
  int count;
  int id;
  PartialOrder *p;

};

#define ALLOCATE_NODE(n) { n = (Node*) calloc(1,sizeof(Node)); assert(n); }
#define FREE_NODE(n)     { if (n) free(n); }


struct edge { 
  Node *source;
  Node *dest;
  double weight;
};

#define ALLOCATE_EDGE(n) { n = (Edge*) calloc(1,sizeof(Edge)); assert(n);}
#define FREE_EDGE(n)     { if (n) free(n); }


struct edges { 
  int max;
  int length;
  Edge *array;
};

#define ALLOCATE_EDGES(n,M) { \
  n = (Edges*) calloc(1,sizeof(Edges)); assert(n);\
  n->array = (Edge*) calloc(M,sizeof(Edge)); assert(n->array); \
  n->max = M;\
 }
#define FREE_EDGES(n)     { \
  if (n) { \
    if (n->array) { free(n->array); n->array=0; }\
    free(n); \
  }\
}



struct tree { 
  int max;
  int length; 
  Node **tree;
  Edge **st;
};

#define ALLOCATE_TREE(t,N) { \
  (t) = (Tree*) calloc(1,sizeof(Tree));\
  (t)->max = N; \
  (t)->tree= (Node**) calloc(N,sizeof(Node*)); \
  (t)->st  = (Edge**) calloc(N,sizeof(Edge*)); \
  assert((t)->tree && (t)->st); \
}


struct forest { 
  int max;
  int length; 
  Tree **forest;
};

#define ALLOCATE_FOREST(e, N) {                 \
 e = (Forest*) calloc(1,sizeof(Forest));         \
 assert(e);                                      \
 (e)->max  = N;                            \
 (e)->forest = (Tree**) calloc((e)->max,sizeof(Tree*)); \
 assert((e)->forest);                                 \
} 
#define FREE_FOREST(e) { \
  if (e)  {\
    if (e->length) {\
        int _i; \
        for (_i=0;_i<e->length;_i++) { FREE_TREE(e->forest[_i]);} \
    }\
    if (e->forest) free(e->forest);\
    free(e);\
  }\
}
 

#define SORT_EDGES(e)  { sortEdges(e->array,0,e->length-1); if (debug) verifyE(e->array,e->length); }




#ifndef MST_MODULE
  
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  
  extern void  sortEdges(Edges * array, int lower, int upper);
  extern Tree *kruskall(TimeSeries *ts, Forest *forest, Edges *e);

  extern int BFSfromASpanningTree(PQueue *queue, 
				  Chain_Heads *breath, 
				  int n,
				  int maxcode,
				  int *N);
  extern int topologicalOrderByMST(Window *w, int *len); 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif // POSET_MODULE










#endif
