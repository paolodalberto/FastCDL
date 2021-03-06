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
#define POSET_MODULE
#define GETTIME
#include <type.h>
#include<poset.h> 
#include <stdlib.h>
#include <string.h>


static int debug = 0;
static int debug2 = 0;





void FREE_POSET(PartialOrder **p, int len) { 

  int i; 
  if ( !p || len==0) return;

  for (i=0;i<len;i++) { 
    if (p[i]) { 
      if (p[i]->t)  { 
	int j;
	if (debug || debug2) { PRINTF("Single Point %d \n",p[i]->common_points); }
	for (j=0;j<p[i]->common_points;j++) { 
	  if (debug || debug2) { PRINTSP(*(p[i]->t[j]));PRINTF(" \n"); }
	  if (/*p[i]->t[j] &&*/ 
	      p[i]->t[j]->code>MAXCODE) // this point is not coming from the input  
	    FREE_SP(p[i]->t[j]);        // we need to free it manually 
	  p[i]->t[j]=0;
	}
	
	free(p[i]->t); 
	p[i]->t=0; 
      }
      free(p[i]->outlinks); 
      p[i]->outlinks=p[i]->inlinks=0;
      free(p[i]->nonEmptyRelations);
      p[i]->nonEmptyRelations=0;
      free(p[i]);
      p[i] =0;
    }
  }
}


void FREE_CHAIN( Chain_Heads *p) { 
  if (p) { 
    if (p->smaller) { 
      FREE_CHAIN(p->smaller);
      //free(p->smaller);
      p->smaller =0;
    }
    if (p->chain) { 
      free(p->chain);
      p->length =0;
      p->chain=0;
    }
    free(p);
  }
  
}

void ADD_OUTLINK(PartialOrder *p, PartialOrder *o) {
  p->outlinks[p->out]= o;
  (p->out)++;	     
  o->inlinks [o->in] = p; 
  (o->in)++;					

}




/********
 * the list is sorted using the stronger order x<y if E i>0 so that for all 
 * j<=i, this will imply that the x<y  (every components will be less) are on the left.
 *
 */ 


static inline void top_and_bottom(TimeSeries *ts, SinglePoint *t, SinglePoint *b) {

  int i;
  for (i=0; i <ts->length;i++) { 
    bottom(b, ts->data+i);
    top(t,    ts->data+i);
    
    if (debug) { 
      PRINTSP(*b); PRINTF(" <-- bottom \n");
      PRINTSP(*t); PRINTF(" <-- top \n");
    }

  }
}


static 
PartialOrder *linearSearchBackward( PartialOrder **chain,  
				    PartialOrder *e, 
				    int len, 
				    int dominates) { 



  int i; 
  for (i=len-1;i>0; i--) {
    if ((dominates)?
      (DOMINATES_PO(*(e->t[0]),*(chain[i]->t[0]))):
      (DOMINATES_PO(*(chain[i]->t[0]),*(e->t[0])))) {
      break;

    }
  }
  return chain[i];

}






static 
PartialOrder *binarySearch( PartialOrder **chain,  
			    PartialOrder *e, 
			    int len) { 
  int next;
  int comp;
  PartialOrder *t;
  next =  len/2; 


  if (debug) { PRINTF(" binarysearch %d", len); } 
  
  if (!len) 
    return 0;
  
  if (debug) { 
    PRINTSP(*(e->t[0])); 
    PRINTF(" vs "); 
    PRINTSP(*(chain[next]->t[0]));
    PRINTF("\n"); 
  } 

  comp = (DOMINATES_PO(*(e->t[0]),*(chain[next]->t[0])));
  
  
  if (len==1) 
    return  (comp)?chain[0]:0;
  else {

    if (comp) {
      t = binarySearch(chain+next,e,len - len/2);
      return (t)?t: chain[next];
    }
    else { 
      t  = binarySearch(chain,e,next); 
      return t;
    }
  }
} 

static 
PartialOrder *DominantSearch( PartialOrder **chain,  PartialOrder *e, int len) { 
  PartialOrder *temp;


  if (debug) { 
    int i;
    PRINTF(" DominantSearch %d \n",len);
    
  }


  if (DOMINATES_PO(*(e->t[0]) ,*(chain[len -1]->t[0]))) {
    return chain[len-1];
  } 
  else return binarySearch(chain,e,len-1);

}


static 
SinglePoint *minimum_neighbor(PartialOrder **out, int len, int k) { 

  SinglePoint *sp; 
  int i=0;
  
  if (len<1) return 0;
  
  ALLOCATE_SP(sp,k);
  { SinglePoint *d = sp;
    SinglePoint *s = out[i]->t[0];
    Mat *_temp=(*d).y;
    
    (*d) = (*s);  
    (*d).y = _temp; 
    memcpy((*d).y,(*s).y,(*s).dimensions*sizeof(Mat)); 
  } 
  for (i=1;i<len;i++) { 
    (*sp) = SP_min(sp,*sp,*(out[i]->t[0])); 
  }
  
  return sp;

} 

static 
int minimum_number_of_neighbor(PartialOrder **out, int len, int code) { 

  int i;
  int u =0;
  int j;
  
  for (i=0;i<len;i++) {
    for (j=0;j<out[i]->common_points;j++) 
      u +=  (out[i]->t[j]->code == code);
  }
  
  return u;

} 

#define INDENT(i) { int _k; for (_k=0;_k<i;_k++) PRINTF(" "); }

int PrintPo(PartialOrder *p, int visit) { 

  int i; 
  static int level=0; 
  if (!p || p->print == visit) return 0;
  
  p->print = visit;

  /*
  INDENT(level+2);PRINTF("-neghbours %d\n",p->out);
  for (i=0;i<p->out;i++) {
    int j;
    PartialOrder *negh = p->outlinks[i];
    INDENT(level+2);PRINTF("--- id %d code %d count %d ",negh->id,negh->code, negh->count);
    for (j=0;j<negh->common_points;j++) { 
      PRINTSP(*(negh->t[j])); 
      PRINTF("pdf %e cdf %e \n",negh->t[j]->pdf, negh->t[j]->cdf);
    }
  }
  */
  
  for (i=0;i<p->out;i++) {
    level +=1;
    PrintPo(p->outlinks[i], visit);
    level-=1;
  }

  INDENT(level); PRINTF("id %d code %d count %d ",p->id,p->code, p->count);
  for (i=0;i<p->common_points;i++) { 
    PRINTSP(*(p->t[i])); 
    PRINTF("pdf %e cdf %e ",p->t[i]->pdf, p->t[i]->cdf);
  }
  PRINTF("\n");
  return 1;

}
int PrintPoAll(PartialOrder *p, int n) { 

  PQueue *queue;
  PartialOrder *cur;
  int i;

  if (!n) return 0;

  PRINTF("Print po All function\n");

  //  debug=1;

  ALLOCATECIRCULARQUEUE(queue,n)
  IN(queue,p);
  
  p->print =1;
  while (!EMPTY(queue)) {
    
    int all_visited=2;
    OUT(queue,cur);   // choose from the top of the list the new vertex

    for (i=0;i<cur->out ;i++) { 
      PartialOrder *prev = cur->outlinks[i]; // for each previous node
      if (!(prev->print&2))  all_visited = 1; 
    }

    if ( all_visited &2) {
      cur->print |= all_visited;

      PRINTF("id %u code %u count %u visit %u",cur->id,cur->code, cur->count, cur->visit);
      for (i=0;i<cur->common_points;i++) { 
	PRINTSP(*(cur->t[i])); 
	PRINTF("pdf %e cdf %e ",cur->t[i]->pdf, cur->t[i]->cdf);
      }
      PRINTF("\n");

      
      for (i=0;i<cur->in;i++) { // for each edge in this vertex  
	if (!(cur->inlinks[i]->print&1)) {  // if it is not marked 
	  if (debug) { 
	    PRINTF(" \t marking  "); PRINTSP(*(cur->inlinks[i]->t[0]))  PRINTF(" prev \n");
	  } 
	  
	  
	  cur->inlinks[i]->print  = 1;           // mark it
	  IN(queue,cur->inlinks[i]);    // queue it
	}
      }

    }
    
    else { 
      if (debug) {
	PRINTF(" PUTTING BACK PrintPoAll"); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d Printtoall \n",cur->visit, queue->top, queue->bottom);
      }

      IN(queue,cur);
      
      //debug=0;
    }
  }
  
  FREECIRCULARQUEUE(queue);
  //  debug =0;
  return 1;

}
  


int PrintChain(Chain_Heads *c) { 

  int i; 
  
  PRINTF("Chain :\n");
  for (i=0; i<c->length;i++) { 
    PartialOrder *p = c->chain[i];
    if (p) { 
      int j;
      PRINTF("id %d code %d count %d ",p->id,p->code, p->count);
      for (j=0;j<p->common_points;j++) { 
	PRINTSP(*(p->t[j])); 
	PRINTF("pdf %e cdf %e ",p->t[j]->pdf, p->t[j]->cdf);
      }
      PRINTF("\n");

    }

    //PrintPo(c->chain[i],c->chain[i]->visit +1);
  }
  
  //  PRINTF("Next Chain :");
  if (c->smaller) PrintChain(c->smaller);
  
  PRINTF("<- Chain \n");
  return 1;

}


int SetPo(PartialOrder *p, int visit) { 

  int i; 
  
  if ( !p || p->visit  == visit ) return 0;
  
  p->visit = visit;
 

  for (i=0;i<p->out;i++) {                    
    SetPo(p->outlinks[i], visit);
  }

  return 1;

}

int SetPrint(PartialOrder *p) { 

  int i; 
  
  if ( !p ||  !p->print ) return 0;
  
  p->print = 0;

  for (i=0;i<p->out;i++) {                    
    SetPrint(p->outlinks[i]);
  }

  return 1;

}


SinglePoint *findNearest(PartialOrder *p, unsigned int code,int max) { 
  int i,j;
  SinglePoint *result = 0;
  if (!p) return 0;
  
  for (i=0;i<p->out;i++) 
    for (j=0;j<p->outlinks[i]->common_points;j++) 
      if (p->outlinks[i]->t[j]->code == code) return p->outlinks[i]->t[j];
  
  
  max--;
  if (max)  
    for (i=0;i<p->out && !result;i++) 
      result = findNearest(p->outlinks[i],code,max);
  else 
    return p->t[0];
    
  
  return result;

}


SinglePoint *findNearest2(PartialOrder *p, unsigned int code, int max) { 
  int i,j;
  SinglePoint *result = 0;
  if (!p) return 0;
  
  for (i=0;i<p->in;i++) 
    for (j=0;j<p->inlinks[i]->common_points;j++) 
      if (p->inlinks[i]->t[j]->code == code) return p->inlinks[i]->t[j];
  
  max--;
  if (max>0) 
    for (i=0;i<p->in && !result;i++) 
      result = findNearest2(p->inlinks[i],code,max);
  else 
    return p->t[0];
  
  
  return result;

}


Chain_Heads *allocate_chains(PartialOrder *e, 
			     unsigned int max) {
  
  Chain_Heads *c = (Chain_Heads*) calloc(1,sizeof(Chain_Heads));                 
  assert(c);                                                        
  c->chain = (PartialOrder **) calloc(max, sizeof(PartialOrder *)); 
  assert(c->chain);                                                 
  c->chain[0] = e;                                                  
  c->length ++;                                                     
  
  return c;
}





Chain_Heads  *COMBINE_DOMINATED_CHAIN(Chain_Heads *e,
				      PartialOrder *d) { 
  
  
  
  int i;

  e->chain[e->length] = d;
  e->length ++;
  
  
  return e;
  
}



/*******************************
 * CDF and PDF are computed in a reasonable fashion 
 * PDF is an estimate by number of points / minimum area of the neighbor
 *
 */

static  
unsigned int distributionFunctionFrom(PartialOrder *p, 
				      double *df, 
				      double *pdf,  
				      int *cur,
				      int N,
				      int visit) {  
  
  int i;
  unsigned int result = p->count;
  int n;
  double dist;
  SinglePoint *min;
  int neigh;

  if (!p || p->visit  )        return 0; 

  if (debug) {
    PRINTSP( *(p->t[0]));PRINTF(" <--\n "); 
  }
  
  
  if (p->out && p->t[0]->code< MAXCODE) { 
    if (debug) { 
      for (i=0;i<p->out;i++) {
	PRINTF(" NEGH  "); PRINTSP( *(p->outlinks[i]->t[0]));
      }
      PRINTF(" \n"); 
    }

    neigh =  minimum_number_of_neighbor(p->outlinks,p->out,p->t[0]->code);
    if (neigh) { 



      min = minimum_neighbor(p->outlinks,p->out, p->t[0]->dimensions);
      dist =  euclidean_distance(*min, *(p->t[0]));
      
      
      
      p->t[0]->pdf  =  ((double) p->count + neigh)/(N*dist);
      
      if (debug) {
	PRINTSP( *(p->t[0]));PRINTF(" MIN  "); 
	PRINTSP( *min);
	PRINTF(" Count %d negh # %d dist %e N %d PDF %e \n", 
	       p->count, neigh,dist,N,  p->t[0]->pdf);
	
	}
      
      FREE_SP(min);
    } 
    else 
      p->t[0]->pdf=0;
      
  } 
  else { 
    p->t[0]->pdf=0;
  }
  


  for (i=0;i<p->out;i++) {
    
    result +=   distributionFunctionFrom(p->outlinks[i],df,pdf,cur,N,visit); 
    
  }
  
  
  if (p->t[0]->code>MAXCODE ) {   p->visit =1;  return 0; }  


  if (debug) {

    PRINTF("cur %d  <--\n ",*cur); 
  }


  df[*cur]  = p->t[0]->cdf = ((double) result)/ N ;
  pdf[*cur] = p->t[0]->pdf;
  
  for (i=0;i<p->common_points;i++) { 
    p->t[i]->pdf = p->t[0]->pdf;
    p->t[i]->cdf = p->t[0]->cdf;
  }

 
  (*cur)++;
   p->visit =  *cur ;
 
  return  result;

}


static int
BFSfromTheBottom(PartialOrder *p, 
		 int n,
		 int maxcode,
		 int *N) { 
  int top=0;
  int bottom=0;
  int i,j,k,l;
  int count =0;

  //debug = 1;
  
  PQueue *queue;
  PartialOrder *cur;
  ALLOCATECIRCULARQUEUE(queue,n)
  IN(queue,p);

  
  p->visit = 1;    // mark the node

  while (!EMPTY(queue)) { 
    int all_visited=2;
    OUT(queue,cur);   // choose from the top of the list the new vertex
    //    if (count>20) debug=0;
   
    //    if (cur->t[0]->x == 4984)  debug=1;

    if (debug) { 
      PRINTF(" HANDLING BFS "); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
     
    } 
    for (i=0;i<cur->out ;i++) {  // what to bring from the predecessors
      PartialOrder *prev = cur->outlinks[i]; // for each previous node
      
      if (debug) { 
	PRINTF(" \t prev  "); PRINTSP(*(prev->t[0]));  
	PRINTF("visit  %d cdf %e prev \n",prev->visit,prev->t[0]->cdf);
      } 
      
      
      
      if (!(prev->visit&2))  all_visited = 1;  // predecessors are not fully visited so this node cannot be either

      // visited and not yet imported 
      if ((prev->visit&2) && !cur->relations[prev->id] && cur->t[0]->code<maxcode) {   // this is not set here but I must set it now
	
	if (debug) { 
	  PRINTF(" \t \t visited prev  "); PRINTSP(*(prev->t[0]))  PRINTF("visit  %d cdf %e prev \n",prev->visit,prev->t[0]->cdf);
	} 
	
	// carry on information about this node
	cur->relations[prev->id] = prev;
	cur->nonEmptyRelations[cur->numberOfRelation++] = prev->id;
	
	for (k=0;k<cur->common_points;k++) 
	  for (l=0;l<prev->common_points;l++) { 
	    
	    if (cur->t[k]->code ==  prev->t[l]->code) { 
	      if (debug) {
		
		PRINTF(" Adding ");  PRINTSP(*(prev->t[l]));
		PRINTF(" to     ");  PRINTSP(*(cur->t[l]));
		PRINTF(" \n");
	      }
	
	      cur->t[k]->cdf += 1.0/ N[cur->t[k]->code];
	    
	    }
	  }
	
	// and all predecessors
		
	for (j=0;j<prev->numberOfRelation;j++) { // take the signature and propagate
	  PartialOrder *carry = prev->relations[prev->nonEmptyRelations[j]];
	  if (!cur->relations[carry->id]) {   // this is not set here but I must set it now
	    cur->relations[carry->id] = carry;
	    cur->nonEmptyRelations[cur->numberOfRelation++] = carry->id;
	    
	    for (k=0;k<cur->common_points;k++) 
	      for (l=0;l<carry->common_points;l++) 
		if (cur->t[k]->code ==  carry->t[l]->code) { 
		  if (debug) {
		    
		    PRINTF(" \t Pred Adding ");  PRINTSP(*(carry->t[l]));
		    PRINTF(" to     ");  PRINTSP(*(cur->t[k]));
		    PRINTF(" \n");
		  }

		  cur->t[k]->cdf += 1.0/ N[cur->t[k]->code];

		}
	    
	  }
	  
	} //for (j=0;j<prev->numberOfRelation;j++) 
	
      } //if ((cur->visit&2) && !cur->relation[prev->id])
      
    }
    
    if ( all_visited &2) {    
      cur->visit |= all_visited;      // visited the vertex
      
      if (cur->t[0]->code<maxcode) 
	for (k=0;k<cur->common_points ;k++) {
	  cur->t[k]->cdf += 1.0/ N[cur->t[k]->code];

	  if (cur->t[k]->cdf>1) { 
	    PRINTF("ERROR "); PRINTSP(*(cur->t[k])); PRINTF("ERROR CDF >1 \n;"); 

	  }

	}
      
      count ++;
      if (debug) { 
	PRINTF(" VISITED   "); PRINTSP(*(cur->t[0]))  PRINTF("visit  %d cdf %e BFS %d \n",cur->visit,cur->t[0]->cdf,count );
	
      } 
      

      for (i=0;i<cur->in;i++) { // for each edge in this vertex  
	if (!(cur->inlinks[i]->visit&1)) {  // if it is not marked 
	  if (debug) { 
	    PRINTF(" \t marking  "); PRINTSP(*(cur->inlinks[i]->t[0]))  PRINTF(" prev \n");
	  } 

	  
	  cur->inlinks[i]->visit |= 1;           // mark it
	  IN(queue,cur->inlinks[i]);    // queue it
	}
      }
      /*
      if (cur->t[0]->x == 4984) {
	debug=0;
	PRINTF(" Print Po All \n");
	PrintPo(cur,0);
	PrintPoAll(cur,n);
      }
      */
    } 
    else { 
      if (debug) {
	PRINTF(" PUTTING BACK BFSfromTheBottom"); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
      }

      IN(queue,cur);
    }
    
  }
  
  FREECIRCULARQUEUE(queue);

  //debug=0;

  return 1;
}


static int
BFSfromTheTop(PartialOrder *p,
	      Chain_Heads *breath,
	      int n,
	      int maxcode,
	      int *N) { 
  int top=0;
  int bottom=0;
  int i,j,k,l;
  unsigned int count =0;

  //debug = 1;
  
  PQueue *queue;
  PartialOrder *cur;
  ALLOCATECIRCULARQUEUE(queue,n)
  IN(queue,p);
  p->count = 0;
  
 
  
  p->visit = 1;    // mark the node

  while (!EMPTY(queue)) { 
    int all_visited=2;
    OUT(queue,cur);   // choose from the top of the list the new vertex
    //    if (count>20) debug=0;
   
    //    if (cur->t[0]->x == 4984)  debug=1;

    if (debug) { 
      PRINTF(" HANDLING BFS "); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
     
    } 
    for (i=0;i<cur->in ;i++) {  // what to bring from the predecessors
      PartialOrder *prev = cur->inlinks[i]; // for each previous node
      
      if (debug) { 
	PRINTF(" \t prev  "); PRINTSP(*(prev->t[0]))  
				PRINTF("visit  %d cdf %e prev \n",prev->visit,prev->t[0]->cdf);
      } 
      
      
      
      if (!(prev->visit&2))  all_visited = 1;  // predecessors are not fully visited so this node cannot be either

      // visited and not yet imported 
      if ((prev->visit&2) && !cur->relations[prev->id] && cur->t[0]->code<maxcode) {   // this is not set here but I must set it now
	
	if (debug) { 
	  PRINTF(" \t \t visited prev  "); PRINTSP(*(prev->t[0]))  PRINTF("visit  %d cdf %e prev \n",prev->visit,prev->t[0]->cdf);
	} 
	
	// carry on information about this node
	cur->relations[prev->id] = prev;
	cur->nonEmptyRelations[cur->numberOfRelation++] = prev->id;
	
	for (k=0;k<cur->common_points;k++) 
	  for (l=0;l<prev->common_points;l++) { 
	    
	    if (cur->t[k]->code ==  prev->t[l]->code) { 
	      if (debug) {
		
		PRINTF(" Adding ");  PRINTSP(*(prev->t[l]));
		PRINTF(" to     ");  PRINTSP(*(cur->t[l]));
		PRINTF(" \n");
	      }
	
	      cur->t[k]->cdf += 1.0/ N[cur->t[k]->code];
	    
	    }
	  }
	
	// and all predecessors
		
	for (j=0;j<prev->numberOfRelation;j++) { // take the signature and propagate
	  PartialOrder *carry = prev->relations[prev->nonEmptyRelations[j]];
	  if (!cur->relations[carry->id]) {   // this is not set here but I must set it now
	    cur->relations[carry->id] = carry;
	    cur->nonEmptyRelations[cur->numberOfRelation++] = carry->id;
	    
	    for (k=0;k<cur->common_points;k++) 
	      for (l=0;l<carry->common_points;l++) 
		if (cur->t[k]->code ==  carry->t[l]->code) { 
		  if (debug) {
		    
		    PRINTF(" \t Pred Adding ");  PRINTSP(*(carry->t[l]));
		    PRINTF(" to     ");  PRINTSP(*(cur->t[k]));
		    PRINTF(" \n");
		  }

		  cur->t[k]->cdf += 1.0/ N[cur->t[k]->code];

		}
	    
	  }
	  
	} //for (j=0;j<prev->numberOfRelation;j++) 
	
      } //if ((cur->visit&2) && !cur->relation[prev->id])
      
    }
    
    if ( all_visited &2) {
      
      int countmax = -1;
      cur->visit |= all_visited;      // visited the vertex
      
      for (i=0;i<cur->in ;i++) {
	int counttemp  = (int) cur->inlinks[i]->td;
	countmax = (countmax<=counttemp)?counttemp:countmax;
	
      }
      cur -> td = countmax +1;

      if (count == cur -> td)
	breath = COMBINE_DOMINATED_CHAIN(breath,cur);
      else { 
	Chain_Heads *temp;
	ALLOCATE_CHAINS(temp,cur,n);
	INSERT_CHAIN(breath,temp);
	breath = temp;
	count ++;
      }
	  

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


      if (cur->t[0]->code<maxcode) 
	for (k=0;k<cur->common_points ;k++) {
	  cur->t[k]->cdf += 1.0/ N[cur->t[k]->code];

	  if (cur->t[k]->cdf>1) { 
	    PRINTF("ERROR "); PRINTSP(*(cur->t[k])); PRINTF("ERROR CDF >1 \n;"); 

	  }

	}
      

      if (debug) { 
	PRINTF(" VISITED   "); PRINTSP(*(cur->t[0]))  PRINTF("visit  %d cdf %e BFS %d \n",cur->visit,cur->t[0]->cdf,count );
	
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
      /*
      if (cur->t[0]->x == 4984) {
	debug=0;
	PRINTF(" Print Po All \n");
	PrintPo(cur,0);
	PrintPoAll(cur,n);
      }
      */
    } 
    else { 
      if (debug) {
	PRINTF(" PUTTING BACK BFSfromTheTop"); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
      }

      IN(queue,cur);
    }
    
  }
  
  FREECIRCULARQUEUE(queue);

  //debug=0;

  return 1;
}

static int
BFSfromTheTop2(PartialOrder *p,
	       Chain_Heads *breath,
	       int n,
	       int maxcode,
	       int *N) { 
  int top=0;
  int bottom=0;
  int i,j,k,l;
  unsigned int count =0;
  unsigned int maxparallel = 0;
  
  //debug = 1;
  
  PQueue *queue;
  PartialOrder *cur;
  ALLOCATECIRCULARQUEUE(queue,n)
  IN(queue,p);
  p->count = 0;
  
 
  
  p->visit = 1;    // mark the node

  while (!EMPTY(queue)) { 
    int all_visited=2;
    OUT(queue,cur);   // choose from the top of the list the new vertex
    //    if (count>20) debug=0;
   
    //    if (cur->t[0]->x == 4984)  debug=1;

    if (debug) { 
      PRINTF(" HANDLING BFS "); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
     
    } 
    for (i=0;i<cur->in ;i++) {  // what to bring from the predecessors
      PartialOrder *prev = cur->inlinks[i]; // for each previous node
      
      if (debug) { 
	PRINTF(" \t prev  "); PRINTSP(*(prev->t[0]))  ;
	PRINTF("visit  %d cdf %e prev \n",prev->visit,prev->t[0]->cdf);
      } 
      
      if (!(prev->visit&2))  all_visited = 1;  // predecessors are not fully visited so this node cannot be either

      
    }
    
    if ( all_visited &2) {
      

      // first we create the topological oprder 
      int countmax = -1;
      cur->visit |= all_visited;      // visited the vertex
      

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
      //#endif

      if (debug) { 
	PRINTF(" VISITED   "); PRINTSP(*(cur->t[0]))  PRINTF("visit  %d cdf %e BFS %d \n",cur->visit,cur->t[0]->cdf,count );
	
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
      /*
      if (cur->t[0]->x == 4984) {
	debug=0;
	PRINTF(" Print Po All \n");
	PrintPo(cur,0);
	PrintPoAll(cur,n);
      }
      */
    } 
    else { 
      if (debug) {
	PRINTF(" PUTTING BACK BFSfromTheTop2"); PRINTSP(*(cur->t[0]))  PRINTF(" visit %d top = %d  bottom %d BFS \n",cur->visit, queue->top, queue->bottom);
      }

      IN(queue,cur);
    }
    
  }
  
  FREECIRCULARQUEUE(queue);
  if (debug) PRINTF("POSET MAX parallel %u ",maxparallel);

  //debug=0;

  return 1;
}




/*******************************
 * CDFs are computed in a reasonable fashion. PDF is an estimate by
 * number of points / minimum area of the neighbor: however if two
 * points from different intervals (codes) are close together one may
 * have PDF equal to zero (no common neighbor) or overestimated
 * because two points are very close (small area) but they belong to
 * different intervals (codes).
 *
 */




static 
int extendedDistributionFunctionFrom3(Chain_Heads *topologicalsort, 
				      double **DF,   // DFs arrays pointers 
				      double **PDF,  // PDFs arrays pointers 
				      int maxcode,   // how many PDFs and CDFs
				      int *cont,      // each node will have a position in hte PDFs/DFs array
				      int *DFspoints,
				      int *N,        // number of points for each series in the poset  (as many maxcode)
				      unsigned int *NE       // number of points for each series  (as many maxcode)
				      ) {


  PQueue *queue;
  PartialOrder *cur;
  int i,j;
  int nu=0; 
  int n=0; 

  for (i=0;i<maxcode;i++) { 
    nu += N[i];
  }


  if (!nu) return 0;
  n = 0;
  while (topologicalsort) {
    for (j=0;j<maxcode;j++) 
      DF [j][n] = 0;
    
    for (j=0;j< topologicalsort->length;j++) { 
      
      cur = topologicalsort->chain[j];
      
      if (cur->t[0]->code < maxcode) {
      
	PartialOrder *p = cur;
	for (i=0;i<p->common_points;i++) { 
	  if ((*cont)>=nu) {
	    PRINTF("ERROR *cur > MAXelements no local ontributions"); 
	    PRINTF(" SP "); PRINTSP(*(p->t[0])); PRINTF("\n");
	  }
	  else { 
	    //      p->t[i]->cdf = DF [(int) p->t[i]->code][*cur] =  ((double)(NE[(int) p->t[i]->code] -startN[(int) p->t[i]->code])) /N[(int)p->t[i]->code];
	    /*DF [(int) p->t[i]->code][*cont] = p->t[i]->cdf; */
	    DF [(int) p->t[i]->code][n] += 1.0;
	    PDF[(int) p->t[i]->code][*cont] = p->t[i]->pdf;
	  }
	  
	}
	

	(*cont) ++;
      }
      
    }
    
    topologicalsort = topologicalsort->smaller;
    n++;
  }

  *DFspoints = n;
  return 1;


}


static 
int dominates_chains(PartialOrder *d, Chain_Heads *e)  {
  
  SinglePoint *ds, *es;
  int dim;
  ds = (d)->t[0];
  es = (e)->chain[e->length -1 ]->t[0];
  dim = MIN(es->dimensions,ds->dimensions);
  
  
  return GE(*ds,*es)==dim;

} 



static 
Chain_Heads * insertInToChainAndPoset(SinglePoint *sp, 
				      Chain_Heads *chain, 
				      PartialOrder **poset,
				      Chain_Heads *cut,
				      int *dc,
				      int N) { 

  int inserted=0; 
  int c =*dc;
  int prevc= c-1;
  Chain_Heads *dominanted    = 0;
  Chain_Heads *newhead    =0;
  Chain_Heads  *prev,*curr;

  if (debug) { 
    PRINTSP(*sp); PRINTF(" Introducing <-- \n");
  }

    
  if (prevc>=0 && poset[prevc] && poset[prevc]->t && 
      EQ(*sp,*(poset[prevc]->t[0])) /*&&
				      sp->code 	== poset[prevc]->t[0]->code*/) { // update the counter and move on: probability 0

    poset[prevc]->count++;
    poset[prevc]->t[poset[prevc]->common_points++]=sp;
    
    if (debug) { 
      PRINTF(" Perfect Match \n");
      PrintPo(poset[prevc], prevc);
    }
  }
  else { // new element
    
    cut->length=0;
      
    prev = chain;
    ALLOCATE_POSET(poset[c],N); // create a poset element 
    poset[c]->t[poset[c]->common_points++] = sp;
    poset[c]->id = c;
    
    // the cut has the dominant node
    cut->chain[cut->length++] = poset[c];
    
    
    // for each chain add the poset 
    for (curr = chain; 
	 curr; 
	 prev=curr, 
	   curr = curr->smaller) {
      
      //      if (debug) {PRINTF("CURR");PrintChain(curr); }
      
      if (dominates_chains(poset[c],curr)) { 
	// each chain has no intersection
	
	cut->chain[cut->length++] = curr->chain[curr->length-1]; 
	curr->chain[0]->code = 1;
	
	
	if (!inserted) {
	  inserted = 1;
	  //	  newhead = allocate_chains(poset[c],N); 
	  
	  curr =  COMBINE_DOMINATED_CHAIN(curr,poset[c]);
	  
	  if (debug) {PRINTF("COMBINED");PrintChain(curr); }
	}
	
	
      }
      else {  // check the dominant node in the chains 
	PartialOrder *dom = DominantSearch(curr->chain, poset[c], curr->length);
	
	
	
	if (dom ) {  // there is a dominant node
	  dom->code = 0;
	  cut->chain[cut->length++] = dom;
	  
	  if (!inserted) { 
	    inserted = 1;
	    ALLOCATE_CHAINS(newhead,poset[c],N); 
	    INSERT_CHAIN(curr,newhead);
	    curr = newhead;
	  }
	  
	}
	else { // there is not, the new chain will be created without ...
	  
	}
      }
      
    } // for curr = chain
    

      // I have a cut, now let's connect only the dominant points 
    if (debug) {PRINTF("CUT");PrintChain(cut);}
    
    { 
				      
      PartialOrder **maps;
      int len = cut->length-1;
      
      int i,j;
      
      maps = ( PartialOrder **) calloc(N+2,sizeof( PartialOrder *));
      
      for (j = len; j>0;j--) { 
	PartialOrder *t = cut->chain[j];
	for (i=0;i<t->out;i++) {
	  maps[t->outlinks[i]->id] = t->outlinks[i];
	  
	}
      }
      
      
      for (j = len; j>0;j--) { 
	
	if (!maps[cut->chain[j]->id]) { 
	  if (debug) { 
	    PRINTF(" Connecting ");  PRINTSP(*(poset[c]->t[0]));
	    PRINTF(" with ");  PRINTSP(*(cut->chain[j]->t[0]));
	    PRINTF(" \n");
	  }
	  ADD_OUTLINK(poset[c],cut->chain[j]);
	}
      }
	
      free(maps);
    
    
    }
      
      
    
    if (debug) { 
      PRINTF(" Finisehd regular introduction  \n");
      PrintPo(poset[c], -1*poset[c]->print);
    }
    c++;
  }
  
  *dc = c; 
  return chain;
}


PartialOrder **partialOrder2(TimeSeries *sortedTS, int *length) { 


  int i,j;
  SinglePoint *t, *b;
  PartialOrder **poset=0;
  Chain_Heads  *chain,*prev,*curr;
  unsigned int len = sortedTS->length+2;
  int lenS = sortedTS->length;
  int c=0;
  int cc=0;
  PartialOrder *last;
  Chain_Heads  *cut ;
  PartialOrder **maps;
      
 
  if (!sortedTS || sortedTS->length==0) return 0;
  
  poset  = (PartialOrder **) calloc(sortedTS->length+2,sizeof(PartialOrder*));
  assert(poset);

  ALLOCATE_CHAINS(cut, 0,sortedTS->length+2);
  chain = prev= curr = 0;
  cut->length = 0;

  ALLOCATE_SP(t,sortedTS->dimensions);
  ALLOCATE_SP(b,sortedTS->dimensions);

  COPYSP(*t,sortedTS->data[sortedTS->length-1]);
  COPYSP(*b,sortedTS->data[0]);
  
   
  top_and_bottom(sortedTS,t,b);
  b->code = MAXCODE+1;
  t->code = MAXCODE+1;


  if (debug) { 
    PRINTSP(*b); PRINTF(" Introducing <-- bottom \n");
    PRINTSP(*t); PRINTF(" <-- top \n");
  }

   
  if (! EQ(sortedTS->data[sortedTS->length-1],*b)) {  // introducing bottom element
    ALLOCATE_POSET(poset[c],sortedTS->length+2);
    
    poset[c]->t[poset[c]->common_points++] = b;
    poset[c]->count =0;
    poset[c]->id = 0;
     
    chain = allocate_chains(poset[c],len);
    prev = chain;
    lenS--;;
    c++;
  

  }  else {                  // the bottom element is from the series
    ALLOCATE_POSET(poset[c],sortedTS->length+2); 

    poset[c]->t[poset[c]->common_points++] = &(sortedTS->data[sortedTS->length-1]);
    poset[c]->count =1;
    poset[c]->id = 0;
    
    chain = allocate_chains(poset[c],len);
    prev = chain;
    c++; 
    lenS -= 2;
    FREE_SP(b);

  } 
      
 
  // for each element in the sorted list: since is in decreasing order from the smallest to the largest
  for (i=lenS; i> -1;i--) { 
    int prevc = c-1;
    SinglePoint *sp = &(sortedTS->data[i]);
    if (debug) { 
      PRINTSP(*sp); PRINTF(" Introducing <-- \n");
    }
    
    
    if (prevc>=0 && poset[prevc] && poset[prevc]->t && 
	EQ(*sp,*(poset[prevc]->t[0]))) { // update the counter and move on: probability 0
      
      poset[prevc]->count++;
      poset[prevc]->t[poset[prevc]->common_points++]=sp;
      
      if (debug) { 
	PRINTF(" Perfect Match \n");
	PrintPo(poset[prevc], prevc);
      }
    }
    else { // new element
      maps   = ( PartialOrder **) calloc(sortedTS->length+2,sizeof( PartialOrder *));
      assert(maps);
      ALLOCATE_POSET(poset[c],sortedTS->length+2); 
      
      poset[c]->t[poset[c]->common_points++] = &(sortedTS->data[i]);
      poset[c]->count =1;
      poset[c]->id = c;

      cut->length=0;
      cut->chain[cut->length++] = poset[c];

      for (j=c-1;j>-1;j--) { 
	if (!maps[poset[j]->id] && DOMINATES_PO(*(poset[c]->t[0]),*(poset[j]->t[0]))) { 
	  int l;
	  ADD_OUTLINK(poset[c],poset[j]);
	  maps[poset[j]->id] = poset[j];
	  for (l=0;l<poset[j]->out;l++) {
	    maps[poset[j]->outlinks[l]->id] = poset[j]->outlinks[l];
	  
	  }
	  
	}
      }
      

      

      free(maps);
      c++;
    }

    
  }


  if (! EQ(sortedTS->data[0],*t)) { 
    if (debug) { PRINTF(" Last element to introduce %d ", c); }

    maps   = ( PartialOrder **) calloc(sortedTS->length+2,sizeof( PartialOrder *));
    assert(maps);
    ALLOCATE_POSET(poset[c],sortedTS->length+2); 
    
    poset[c]->t[poset[c]->common_points++] = t;
    poset[c]->count =1;
    poset[c]->id = c;
    
    cut->length=0;
    cut->chain[cut->length++] = poset[c];
    
    for (j=c-1;j>-1;j--) { 
      if (!maps[poset[j]->id] && DOMINATES_PO(*(poset[c]->t[0]),*(poset[j]->t[0]))) { 
	int l;
	ADD_OUTLINK(poset[c],poset[j]);
	maps[poset[j]->id] = poset[j];
	for (l=0;l<poset[j]->out;l++) {
	  maps[poset[j]->outlinks[l]->id] = poset[j]->outlinks[l];
	  
	}
      }
    }
    

      

    free(maps);
    c++;
  } else {
    FREE_SP(t);
  }

  if (debug) { 
    PrintChain(chain);
    PrintPo(poset[c-1],-1*poset[c-1]->print);

  }

  // free the chain ...   
  FREE_CHAIN(chain);
  FREE_CHAIN(cut);


  
  *length = c;

  return poset;




}

PartialOrder **partialOrder(TimeSeries *sortedTS, int *length) { 

  int i;
  SinglePoint *t, *b;
  PartialOrder **poset=0;
  Chain_Heads  *chain,*prev,*curr;
  unsigned int len = sortedTS->length+2;
  int lenS = sortedTS->length;
  int c=0;
  int cc=0;
  PartialOrder *last;
  Chain_Heads  *cut ;
 
  if (!sortedTS || sortedTS->length==0) return 0;
  
  poset  = (PartialOrder **) calloc(sortedTS->length+2,sizeof(PartialOrder*));
  assert(poset);

  ALLOCATE_CHAINS(cut, 0,sortedTS->length+2);
  chain = prev= curr = 0;

  ALLOCATE_SP(t,sortedTS->dimensions);
  ALLOCATE_SP(b,sortedTS->dimensions);

  COPYSP(*t,sortedTS->data[sortedTS->length-1]);
  COPYSP(*b,sortedTS->data[0]);
  
   
  top_and_bottom(sortedTS,t,b);
  b->code = MAXCODE+1;
  t->code = MAXCODE+1;


  if (debug) { 
    PRINTSP(*b); PRINTF(" Introducing <-- bottom \n");
    PRINTSP(*t); PRINTF(" <-- top \n");
  }

   
  if (! EQ(sortedTS->data[sortedTS->length-1],*b)) {  // introducing bottom element
    ALLOCATE_POSET(poset[c],sortedTS->length+2);
    
    poset[c]->t[poset[c]->common_points++] = b;
    poset[c]->count =0;
    poset[c]->id = 0;
     
    chain = allocate_chains(poset[c],len);
    prev = chain;
    lenS--;;
    c++;
  

  }  else {                  // the bottom element is from the series
    ALLOCATE_POSET(poset[c],sortedTS->length+2); 
    




    poset[c]->t[poset[c]->common_points++] = &(sortedTS->data[sortedTS->length-1]);
    poset[c]->count =1;
    poset[c]->id = 0;
    
    chain = allocate_chains(poset[c],len);
    prev = chain;
    c++; 
    lenS -= 2;
    FREE_SP(b);

  } 

  // for each element in the sorted list: since is in decreasing order from the smallest to the largest
  for (i=lenS; i> -1;i--) { 
    if(debug) { PRINTF(" i %d x %ld ",i,sortedTS->data[i].x); PRINTSP(sortedTS->data[i]);  PRINTF(" Introducing <-- \n"); }
    /*    if (sortedTS->data[i].x == 4944) { 
      debug=1;   
      }*/
    chain = insertInToChainAndPoset(&(sortedTS->data[i]),chain,poset,cut,&c,sortedTS->length+2);
    //if (sortedTS->data[i].x == 4944) debug=0;
  }


  if (! EQ(sortedTS->data[0],*t)) { 
    if (debug) { PRINTF(" Last element to introduce %d ", c); }

    chain = insertInToChainAndPoset(t,chain,poset,cut,&c,sortedTS->length+2);

  } else {
    FREE_SP(t);
  }

  if (debug) { 
    PrintChain(chain);
    PrintPo(poset[c-1],-1*poset[c-1]->print);

  }

  // free the chain ...   
  FREE_CHAIN(chain);
  FREE_CHAIN(cut);
  

  
  *length = c;

  return poset;



}


int topologicalOrderByPoset(Window *w, int *len) {

 
  if (debug) {START_CLOCK;}
  w->poset = partialOrder2(w->sw->sortedts,&w->posetlen);  // build the poset DAG 
  if (debug) {  END_CLOCK; PRINTF("PARTIAL \n");}


  // create the topological order and estimate the PDFs
  if (debug) {  START_CLOCK;}
  ALLOCATE_CHAINS(w->topologicalsort,  0,w->sw->sortedts->length+2);
  w->topologicalsort->length = 0;

  BFSfromTheTop2(w->poset[w->posetlen-1], 
		 w->topologicalsort,
		 w->sw->sortedts->length+2,
		 2,
		 len); 
  
  if (debug) {END_CLOCK; PRINTF("BFS\n");}
  
  if (debug) {
    SetPrint(w->poset[w->posetlen-1]);
    PRINTWINDOW(w, 0);
    if (debug) { PRINTF(" ---------------- \n") ; }
    PrintPo(w->poset[w->posetlen-1], -1*(w->poset[w->posetlen-1]->print));
  }

  
  return 1;

}




/**********************************
 * Variation of the poset-bininsertionsort presented Daskalakis et al. SODA 2008
 * "sorting and selection Posets" (in turn taken from Faigle and turan "sorting and recognition problems for ordered sets" 
 *
 */ 

Window *createStatistics(TimeSeries *ts, int  type_order) { 

  Window *w;
  int i=0,j=0;
  int    len[1] = { ts->length} ;
  double *dfs[1] ;
  double *pdfs[1];
  unsigned int    lenG[1] = { 0} ;
 

  ALLOCATEW(w,ts->max,ts->dimensions);
  assert(w);
  

  COPY_TS(w->ts,ts);
  COPY_TS(w->sw->sortedts,ts);
  
    
  SORT_TIMESERIES(w->sw->sortedts,VALUESLOT); // strong order, my suggestion so I look for dominated nodes only 
  
  dfs[0] = w->sw->df;
  pdfs[0] = w->sw->pdf;
  
  switch (type_order) { 
  case POSET:
    topologicalOrderByPoset(w,len);
    break;
  case MST:
  default:
    topologicalOrderByMST(w,len);
    break;
  }

  if (debug) { 

    printf("topological order done \n");

  }

  if (debug) {START_CLOCK;} 
  extendedDistributionFunctionFrom3(w->topologicalsort,
				   dfs, pdfs,
				   1,
				   &i, &j, len, lenG);

  if (debug) {END_CLOCK;PRINTF("extended \n");}

  w->sw->statistics_len = i;  
  w->sw->statistics_len_df = j;  

  /*
    SORT_TIMESERIES(w->sw->sortedts,VALUESLOT); // strong order, my suggestion so I look for dominated nodes only 
  */
  /*
  distributionFunctionFrom(w->poset[w->posetlen-1],
			   w->sw->df,
			   w->sw->pdf,
			   &i,
			   ts->length,
			   0);        // visit the DAG above and accumulate the DF, because I cannot see an easy way to accumulating while building .....
  
  w->sw->statistics_len = i;
  if (debug) { 
    PrintPoAll(w->poset[w->posetlen-1],0);
    PRINTF(" Statistics using the poset ");
    PrintPo(w->poset[w->posetlen-1],-1*w->poset[w->posetlen-1]->print);
    PRINTF("######  statistic lenght %d \n",i);
  }
  */

  SORT_ARRAY(w->sw->pdf,w->sw->statistics_len);
  
  return w;
}


/**********************************
 * Variation of the poset-bininsertionsort presented Daskalakis et al. SODA 2008
 * "sorting and selection Posets" (in turn taken from Faigle and turan "sorting and recognition problems for ordered sets" 
 *
 */ 








Window *createExtendedStatistics(TimeSeries *ref, TimeSeries *win, int  type_order ) { 

  int i=0;
  double *dfs[2] ;
  double *pdfs[2];
  int    len[2] = { ref->length, win->length} ;
  unsigned int    lenG[2] = { 0, 0} ;
  int code;
  int j;
  Chain_Heads *topologicalsort;

  Window *w;
  TimeSeries *f;




  if (debug) { 
    PRINTTIMESERIES(ref,0);
    PRINTTIMESERIES(win,0);
  }
  if (debug) { PRINTF(" ---------------- \n") ; }

  /* ALLOCATEW(w,ref->max+win->max,ref->dimensions); */
  w = allocatew(ref->max+win->max,ref->dimensions);





  dfs[0] = w->sw->df;
  dfs[1] = w->sw->ext_df;

  pdfs[0] = w->sw->pdf;
  pdfs[1] = w->sw->ext_pdf;

  if (debug) { 
    if (debug) { PRINTF(" WINDOW --------------- \n") ; }
    PRINTWINDOW(w,0);
  }




  COPY_TS(w->ts,ref);
  COPY_TS(w->sw->sortedts,ref);

  if (debug) { 
    PRINTWINDOW(w,0);
  }

  append_ts(w->ts,win);

  if (debug) { 
    PRINTWINDOW(w,0);
  }

  append_ts(w->sw->sortedts,win);
  if (debug) { 
    PRINTWINDOW(w,0);
  }

  if (debug) {START_CLOCK;}
  SORT_TIMESERIES(w->sw->sortedts,VALUESLOT); // strong order, my suggestion so I look for dominated nodes only 
  if (debug) {END_CLOCK;PRINTF("SORT\n");}
  if (debug) { 
    PRINTWINDOW(w,0);
  }



  switch (type_order) { 
  case POSET:
    topologicalOrderByPoset(w,len);
    break;
  case MST:
  default:
    topologicalOrderByMST(w,len);
    break;
  }

  if (debug) {  START_CLOCK;}
  extendedDistributionFunctionFrom3(w->topologicalsort,
				   dfs, pdfs,
				   2,
				   &i, &j, len, lenG);

  if (debug) {  END_CLOCK;PRINTF("extended \n");}

  w->sw->statistics_len = i;  
  w->sw->statistics_len_df = j;  
 
  //  SORT_ARRAY(pdfs[0],i);  
  //SORT_ARRAY(pdfs[1],i);  

  if (debug) {
    SetPrint(w->poset[w->posetlen-1]);
    PRINTWINDOW(w, 0);
    PRINTF(" POSET ---------------- \n") ; 
    //PrintPoAll(w->poset[w->posetlen-1], w->posetlen);
  }
  /*else SetPo(w->poset[w->posetlen-1], 0); */
  


  /*
  code = (int) ref->data[0].code;
  for (i=0;i<ref->length;i++) { 
    dfs[code][i] =  dfs[code][i]/ (double) ref->length;
  }
  code = (int) win->data[0].code;
  for (i=0;i<win->length;i++) { 
    dfs[code][i] =  dfs[code][i]/ (double) win->length;
  } 
  */
  return w;
}
