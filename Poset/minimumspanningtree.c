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
#define MST_MODULE 1
#define GETTIME
#include <type.h>
#include <minimumspanningtree.h>
#include <sort.h>
#include<distance.h>
#include<poset.h>

static int debug =0;

static 
void FREE_TREE(Tree *t) { 
  if (t) { 
    if (t->length) { 
      int _i; 
      for (_i=0;_i<t->length;_i++) { FREE_NODE(t->tree[_i]);} 
      //for (_i=0;_i<t->length-1;_i++) { FREE_EDGE(t->st[_i]);} 
    } 
    if (t->tree) { free(t->tree); t->tree=0;} 
    if (t->st)   { free(t->st); t->st = 0; }
    free(t); 
  }
}



int mergeTrees(Tree *t1, Tree *t2, Edge *e) { 

  int i;
  
  if (debug) { 
    printf("t1 length %d t2 length %d \n",t1->length,t2->length);

  }

  t1->st[t1->length-1] = e; 
  for (i=0;i<t2->length-1;i++) { 
    if (debug) { 
      printf(" i %d \n",i);
    }

    t1->tree[t1->length+i] = t2->tree[i];
    t1->tree[t1->length+i]->tree = t1;
    t1->st[t1->length+i] = t2->st[i]; 

  }

  t1->tree[t1->length+i] = t2->tree[i];
  t1->tree[t1->length+i]->tree = t1;
  t1->length += t2->length;
  

  return  t1->length;
}

#define SWAPE(a,i,j) {				\
    Edge   tn;						\
    tn = a[i];					\
    a[i] = a[j];				\
    a[j] = tn;					\
  }


#define GTE(left, i, right) (GTX(left[i].weight,right.weight))
#define LTE(left, i, right) (LTX(left[i].weight,right.weight))
#define GEE(left, i, right) (GEX(left[i].weight,right.weight))
#define LEE(left, i, right) (LEX(left[i].weight,right.weight))
#define EQE(left, i, right) (EQX(left[i].weight,right.weight))
#define NEE(left, i, right) (NEX(left[i].weight,right.weight))

#ifdef DECREASING_ORDER_E
#undef DECREASING_ORDER_E
#endif


#if(DECREASING_ORDER_E) 
#define BBE(i,j,src,pivot,lower,upper) { if (debug) {PRINTF(" DECREASING \n"); }	\
    do { (i)++; }                                                                       \
    while ((i)<=(upper) && GTE(src,i,pivot));	                                        \
    do {(j)--; }                                                                        \
    while ( (j>=lower) && LTE(src,j,pivot));                                            \
  }
#else 
#define BBE(i,j,src,pivot,lower,upper) {		                                \
    do {(i)++; }                                                                        \
    while ((i)<=(upper) && LTE(src,i,pivot));	                                        \
    do {(j)--; }                                                                        \
    while ((j>=lower) && GTE(src,j,pivot) );                                            \
  }
#endif



static 
void  insortEdges (Edge *array, register int lower, register int upper)
{
  register int i, j;
  Edge temp;

  
  for (i = lower; i <= upper; i++) {

    j = i;

    temp   = array[j];
 	       
    while (j >lower  && 
#if(DECREASING_ORDER) 
	   LTE(array,j-1,temp)
#else
	   GTE(array,j-1,temp)
#endif	   
) {
      array[j]   = array[j-1];
      j--;
    }
    array[j]   = temp;
  }

}


#define CUTOFF 16

void  sortEdges(register Edge * array, register int lower, register int upper)
{
  register int        i, j;
  Edge pivot;


  if (debug) { 
    printf("sort %d %d \n",lower,upper);
  }

  if (upper - lower > CUTOFF) {
    SWAPE(array,lower, (upper+lower)/2);
    i = lower;  j = upper+1;  

    pivot = array[lower];
    
    while (1) {
      BBE(i,j,array,pivot,lower,upper);
      if (j <= i) break;
      SWAPE(array,i,j);
    }

    SWAPE(array,lower,j);
    sortEdges(array, lower, j - 1);
    sortEdges(array, i, upper);
  
  }
  else { 
    if (upper-lower>=1)
      insortEdges(array,lower,upper);
  }

}

int verifyE(Edge * array, int l) { 

    int i;
  int e=1;

  for (i=1;i<l;i++) { 
    if ( !
#if(DECREASING_ORDER) 
	LEE(array,i,array[i-1])
#else
	GEE(array,i,array[i-1])
#endif	
	) 
      { 
	
	printf("%e %e failed comparison at %d \n",array[i].weight, array[i-1].weight,i);
	e = 0;


      }
    else if (debug) printf("%e %d \n",array[i-1].weight,i);

 
  }
  printf("\n");
  
  return e;


}

static 
int buildGraph(TimeSeries *ts,  Forest *forest, Edges  *edges,PartialOrder **poset , DistanceFunctions dist) {

  int i,j;
  int e=0;
  int no=0;
  int dimensions;
  int manual =1;
  Tree *t;
  //  PRINTF("START building graphR \n");
  Node **n = (Node**) calloc(ts->length,sizeof(Node*));
  assert(n);
  
  //PRINTF("START building graphR \n");

  // first we create the nodes: the connection between trees and posets
  // we assume that the time series is sorted by value in lexicographical order

  if (debug) {START_CLOCK;}

  i = 0;
  ALLOCATE_POSET(poset[no],ts->length);
  poset[no]->t[poset[no]->common_points++] = &ts->data[i];
  poset[no]->count =0;
  poset[no]->id = no;
  
  ALLOCATE_NODE(n[no]);
  n[no]->id =no;
  n[no]->p = poset[no];

  
  
  ALLOCATE_TREE(t,ts->length);
  t->tree[0] = n[no];
  n[no]->tree = t;
  t->length=1;
  forest->forest[no] = t;
  
  no++;
  
  for (i=1;i<ts->length;i++) {
    
    // this element is different from the previous
    if (manual &&  !EQ(ts->data[i],ts->data[i-1]) ) { 
    
      ALLOCATE_POSET(poset[no],ts->length);
      poset[no]->t[(poset[no]->common_points)++] = &ts->data[i];
      poset[no]->count =0;
      poset[no]->id = no;
      ALLOCATE_NODE(n[no]);
      n[no]->id =no;
      n[no]->p = poset[no];

      ALLOCATE_TREE(t,ts->length);
      t->tree[0] = n[no];
      t->length=1;
      n[no]->tree = t;
      forest->forest[no] = t;

      no++;
    }
    else {  // is the same thus add in the common points
      int prev_no = no-1;
      if (debug) {
	PRINTSP(ts->data[i]);
	PRINTF("else %d %d -<\n",poset[prev_no]->common_points,i);
      }
      poset[prev_no]->t[(poset[prev_no]->common_points)++] = &(ts->data[i]);
      /*PRINTF("else -<\n");*/
      (poset[prev_no]->count )++;
      /*PRINTF("else -<\n"); */
      (n[prev_no]->count)++;
      /*      PRINTF("else -<\n");*/
    }
  }
  
  forest->length = no;
  if (debug) {  END_CLOCK; PRINTF("built forest \n");}
  
  // Edge computation
  if (debug) {START_CLOCK;}
  dimensions = n[0]->p->t[0]->dimensions; 

  for (i=0; i<no;i++) { 
    SinglePoint *left = n[i]->p->t[0];
    // each node begins being its own tree
    
    

    // compute the edge distance
    for (j=i+1;j<no;j++) { 
      SinglePoint *right= n[j]->p->t[0];
      double d = dist(left->y,right->y,dimensions);
      if (debug) { 
	printf("%d-%d %e \n",i,j,d); 

      }
      edges->array[e].source = n[i];
      edges->array[e].dest   = n[j];
      edges->array[e].weight = d;
      e++;

    }
  }

  forest->length = i;
  edges->length = e;
  if (debug) {  END_CLOCK; PRINTF("built Edges \n");}
  if (debug) {START_CLOCK;}
  SORT_EDGES(edges);
  if (debug) {  END_CLOCK; PRINTF("SORT Edges \n");}

  // The poset data structure will be used to create the topological
  // order from the leaves of the span tree after the kruskall algorithm

  free(n);
}

 
Tree *kruskall(TimeSeries *ts, Forest *forest, Edges *edges,PartialOrder **poset ) { 

  int ed_count;
  int tree_n = 0;
  Tree   *t;
  

  if (debug) {START_CLOCK;}
  buildGraph(ts,forest,edges,poset,Euclide);
  if (debug) {  END_CLOCK; PRINTF("building graph \n");}
  
  tree_n = forest->length;
  t = forest->forest[0];

  if (debug) {START_CLOCK;}
  for (ed_count=0; ed_count<edges->length && t->length < tree_n ; ed_count++) {
    Edge *min = &edges->array[ed_count];
    
    if (debug) { printf("%d %e \n", ed_count,min->weight);}
    
    if (min->source->tree != min->dest->tree) {
      Tree *temp =  min->dest->tree;
      
      if (debug) { 
	SinglePoint *right= min->source->p->t[0];
	SinglePoint *left= min->dest->p->t[0];
	PRINTSP(*left);
	PRINTSP(*right);
      }
      // add the poset link

      ADD_OUTLINK(min->source->p,min->dest->p);
      ADD_OUTLINK(min->dest->p,min->source->p);
      
      if (debug) { 
	printf("Created connection\n");
      }

      
      // merge the two trees into a single one, the left one
      mergeTrees(min->source->tree, min->dest->tree,min);
      if (debug) { 
	printf("Merge tree\n");
      }

      
      // Free the right one 
      temp->length = 0;
      FREE_TREE(temp);
      if (debug) { 
	printf("Free temp\n");
      }


      // keep the left one as last one standing
      t = min->source->tree;
    }
    
  }
  if (debug) {  END_CLOCK; PRINTF("DONE TREE \n");}
  
  return t;
}





static int
BFSfromASpanningTree(PQueue *queue,
		     Chain_Heads *breath,
		     int n,
		     int maxcode,
		     int *N) { 
  int top=0;
  int bottom=0;
  int i,j,k,l;
  unsigned int count =0;
  unsigned int maxparallel = 0;
  PartialOrder *cur;
  
  while (!EMPTY(queue)) { 
    OUT(queue,cur);  
    cur ->visit = 1;
    if (debug) { 
      PRINTF(" HANDLING BFS "); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
     
    } 
    // first we create the topological oprder 
    int countmax = -1;
    
    // we check the topological distance number
    for (i=0;i<cur->in ;i++) {
      int counttemp  = (int) cur->inlinks[i]->td;
      countmax = (countmax<=counttemp)?counttemp:countmax;
      
    }
    cur -> td = countmax +1;

    if (count == cur -> td) { // same bucket 
      breath = COMBINE_DOMINATED_CHAIN(breath,cur);
      if (maxparallel< breath->length)
	maxparallel = breath->length;
    }
    else { // new bucket 
      Chain_Heads *temp;
      ALLOCATE_CHAINS(temp,cur,n);
      INSERT_CHAIN(breath,temp);
      breath = temp;
      count ++;
    }
	  

    //#ifdef PDFONLY
    // PDF estimation 
    for (i=0;i<cur->common_points ;i++) { 
      if (cur->in && cur->t[i]->code< MAXCODE) { 
	int c = 3;

	SinglePoint *nei2 = findNearest2(cur, cur->t[i]->code,3);
	SinglePoint *nei  = findNearest (cur, cur->t[i]->code,3);
	
	if (!nei) {  nei = cur->t[i]; c--;}
	if (!nei2) { nei2 = cur->t[i]; c--;}
	
	if (c==1)  cur->t[i]->pdf=0;
	else { 
	  cur->t[i]->pdf  =  ((double) c)/ 
	    (N[cur->t[i]->code]*euclidean_distance(*nei, *nei2));
	}
	
      } 
      else  // no sublinks or no internal node: no PDF 
	cur->t[i]->pdf=0;
      
    }
    
    for (i=0;i<cur->out;i++) { // for each edge in this vertex  
      if (!(cur->outlinks[i]->visit&1)) {  // if it is not marked 
	if (debug) { 
	  PRINTF(" \t marking  "); PRINTSP(*(cur->outlinks[i]->t[0]))  PRINTF(" prev \n");
	} 
	
	  
	cur->outlinks[i]->visit |= 1;           // mark it
	IN(queue,cur->outlinks[i]);    // queue it
      }
    }
        
  }
  
  FREECIRCULARQUEUE(queue);
  if (debug) {PRINTF("MST MAX parallel %u ",maxparallel);}

  //debug=0;

  return 1;
}



int topologicalOrderByMST(Window *w, int *len) {
  Tree    *mst;
  Forest  *forest;
  Edges   *edges;
  PQueue  *leaves; 
  int i;

  ALLOCATE_FOREST(forest,(w->
			  sw->
			  sortedts->
			  length));
  ALLOCATE_EDGES(edges, (w->sw->sortedts->length)*
			(w->sw->sortedts->length - 1)/2);


  w->poset  = (PartialOrder **) calloc(w->sw->sortedts->length,sizeof(PartialOrder*));
  assert(w->poset);
  if (debug) {  START_CLOCK;}
  mst = kruskall(w->sw->sortedts,forest,edges,w->poset);  // build the poset DAG 
  if (debug) {END_CLOCK; PRINTF("KRUSKAL \n");}
  w->posetlen = mst->length;
  
  ALLOCATECIRCULARQUEUE(leaves,w->sw->sortedts->length);
  
  for (i=0;i<mst->length;i++)  
    if (mst->tree[i]->p->in == 1  && mst->tree[i]->p->out == 1 ) // for each leaf
      IN(leaves,mst->tree[i]->p);

  // the tree is a connected not oriented graph, so any node will do 
  FREE_TREE(mst);


  // create the topological order and estimate the PDFs
  if (debug) {  END_CLOCK; PRINTF("MST \n");}
  if (debug) {  START_CLOCK;}
  ALLOCATE_CHAINS(w->topologicalsort,  0,w->sw->sortedts->length);
  w->topologicalsort->length = 0;

  BFSfromASpanningTree(leaves, 
		       w->topologicalsort,
		       w->sw->sortedts->length+2,
		       2,
		       len); 
  
  if (debug) {END_CLOCK; PRINTF("BFS\n");}

  

  
  
  
  FREE_EDGES(edges);

  forest->length=0;
  FREE_FOREST(forest);


  return 1;
}
