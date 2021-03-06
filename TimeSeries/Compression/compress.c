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
#include <bzlib.h>



#define COMPRESS_MODULE 1

#include <compress.h>

// from pvalue.c 
extern double PVAL[COL];

static int debug = 0;

KolmogorovState *updatekolstate(KolmogorovState *state, TimeSeries *stream,int N,int M, int BM) {
  
  if (!(state)) {
    ALLOCATE_KOL_STATE(state,N,M,stream->dimensions);                             
    UPDATESTATE((state)->s,stream,N,M);                        
    /*(state)->r = PrintTimeSeriesY((state)->s->r->ts,0,0);*/      
    /*(state)->lr = strlen(state->r); */                           
    (state)->r = (char *)(state)->s->r->ts->data_anchor;      
    (state)->lr = state->s->r->ts->length*stream->dimensions*sizeof(Mat);   
    (state)->BMAX = BM;
  }							         
  else { 
    UPDATESTATE((state)->s,stream,N,M);                   
    if ((state)->s->r->ts->length<N) {			 
      //if ((state)->r) free((state)->r);                    
      /*(state)->r = PrintTimeSeriesY((state)->s->r->ts,0,0);*/      
      /*(state)->lr = strlen(state->r); */                           
      (state)->r = (char*)(state)->s->r->ts->data_anchor;      
      (state)->lr = state->s->r->ts->length*stream->dimensions*sizeof(Mat);                            
    } else /*if ((state)->w->ts->length<M)*/ {		 
      //if ((state)->w) free((state)->w);                    
      /*(state)->w = PrintTimeSeriesY((state)->s->w->ts,0,0);*/ 
      /* (state)->lw = strlen(state->w);                    */  
      (state)->w = (char *)(state)->s->w->ts->data_anchor; 
      (state)->lw = state->s->w->ts->length*stream->dimensions*sizeof(Mat);                      
    }							 
  }
  return state;
}


char *
PrintCompressState(KolmogorovState *s, int identation, int P) { 
  char *temp=0;
  char *val =0;
  if (s && s->s) {  
    char *line;
    if (s->s->r) { 
      int len = s->s->r->ts->max*64*s->s->r->ts->dimensions+identation+128;
      if (debug) { 
	      PRINTF("len %d \n",8*len);
      }
      line  = (char *) calloc(8*len,sizeof(char));
      temp  = (char *) calloc(8*len,sizeof(char));
    }
    else { 
      line  = (char *) calloc((64+identation+128),sizeof(char));
      temp  = (char *) calloc((2*128+3*identation+128),sizeof(char));
    }
    assert(line && temp);
    IDENTS(identation,temp,line);SPRINTNA(temp,line,"SKOL B 0 \n");
    IDENTS(identation,temp,line);SPRINT(temp,line,"Boot max: %d\n",s->BMAX);
    IDENTS(identation,temp,line);SPRINT(temp,line,"Baseline : %d\n",s->baseline);
    for (int i=0;i<COL;i++) { IDENTS(identation,temp,line);SPRINT(temp,line,"B : %e\n",s->boundaries[i]); }
    //IDENTS(identation,temp,line); if (s->r) {SPRINT(temp,line,"R %s \n",s->r);} else {SPRINTNA(temp,line,"R \n");} 
    //IDENTS(identation,temp,line); if (s->w) {SPRINT(temp,line,"W %s \n",s->w);} else {SPRINTNA(temp,line,"W \n");}
    val = PrintNONPState(s->s, 3, 0);
    if (val) { 
      if (debug) { 
	PRINTF(" length of val %ld of hte string %s \n",strlen(val),val) ;
	
      }
      PRINTF("INDENTS \n");
      IDENTS(identation,temp,line);
      strcat(temp,val); 
      free(val);
    }
    if (P==1) PRINTF("%s\n",temp);
    free(line);

  }
  return temp;
}



KolmogorovState *
ReadKOLState(int identation, char **temp) { 
  int size;
  char *running=*temp;
  KolmogorovState *t = (KolmogorovState *) calloc(1,sizeof(KolmogorovState));

  if (running) {  
    int n;
    int data;
    
    running += identation;    
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"SKOL B %u ",&size);
    running += n+1+identation;

    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"Boot max: %d",&(t->BMAX));
    running +=n+1;

    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"Baseline : %d",&(t->baseline));
    running +=n+1;
    
    for (int i=0;i<COL;i++) { FINDSLASHN(running,n); running[n] = 0; sscanf(running,"B : %lf",&(t->boundaries[i]));running +=n+1; }
    t->s = ReadNONPState(identation+3,&running);
    
    (t)->r = (char *)(t)->s->r->ts->data_anchor;      
    (t)->lr = (t->s->r->ts->length)*(t->s->r->ts->dimensions)*sizeof(Mat);   
    
    (t)->w = (char *)(t)->s->w->ts->data_anchor; 
    (t)->lw = (t->s->w->ts->length)*(t->s->w->ts->dimensions)*sizeof(Mat);                      

    //*temp = running;
  }
  return t;

}








unsigned int zlib_compress( char *string, unsigned int len) { 

  bz_stream *bz = (bz_stream*) calloc(1,sizeof(bz_stream));
  int error;
  unsigned int res_len;
  char *temp; 
  int W = 50;


  /*
  bz ->bzalloc = bzalloc;
  bz ->bzfree  = bzfree;
  bz -> opaque = opaque;
  */
  error = BZ2_bzCompressInit ( bz,
			       9 /* 9 blockSize100k best compression*/,
			       0 /*verbosity quite */,
			       W /* workFactor standard */ );

  bz->next_in = string;
  bz->avail_in = len;

  bz->next_out = temp = (char *) calloc(len*W,sizeof(char));
  bz->avail_out = len*W;
  assert(bz->next_out);

  if (error != BZ_OK) return 0;
  
  error =  BZ2_bzCompress (bz, 
			   BZ_RUN /* action */);

  while ( error != BZ_STREAM_END) { 
    error =  BZ2_bzCompress (bz,  BZ_FINISH /* action */);
  }
  
  error = BZ2_bzCompressEnd(bz);
  
  if (error != BZ_OK) return 0;

  res_len = len*W - bz->avail_out;
  
  if (temp) free(temp);
  bz-> next_out = 0;
  free(bz);
  
  return res_len;

}

#ifndef MIN
#define MAX(x,y) ((x>y)?x:y)
#define MIN(x,y) ((x<y)?x:y)
#endif

// LI et al.: THE SIMILARITY METRIC
// IEEE TRANSACTIONS ON INFORMATION THEORY, VOL. 50, NO. 12, DECEMBER 2004

double normalized_compression_difference_measure(char *ref, unsigned int reflen, 
						 char *comp, unsigned int complen, 
						 Compression compf ) { 
  
  unsigned int  rc,r,c;
  double m =0;
  char *concatenation = (char *) calloc(reflen+complen+1, sizeof(char));
  assert(concatenation);
  memcpy(concatenation,ref,reflen);
  memcpy(concatenation+reflen,comp,complen);
  
  rc = compf(concatenation,reflen+complen);
  
  free(concatenation);
  
  r  = compf(ref,reflen);
  c  = compf(comp,complen);
  
  if ( rc ==0 || r ==0 || c == 0) return 1;
  
  m = (double) (rc - MIN(r,c));
  return m / MAX(r,c);
  
}


int bootstrap_compression( KolmogorovState *state, 
			   Compression compf) {
  char *r, *w;
  unsigned int lr,lw;
  int i,j;
  int h,tot;
  double *res = (double*) calloc(state->s->r->ts->length*state->s->w->ts->length,sizeof(double));
  int *count = (int*) calloc(state->s->r->ts->length*state->s->w->ts->length,sizeof(int));
  double reference; 
  SinglePoint *ts,*ws;
  
  assert(res);
  ALLOCATE_SP(ts,state->s->r->ts->dimensions);
  ALLOCATE_SP(ws,state->s->w->ts->dimensions);
  
  h=0;
  
  j = MIN(state->s->r->ts->length,state->s->w->ts->length);
  
  if (j> state->BMAX) 
    j = state->BMAX;

  reference = res[h++] = normalized_compression_difference_measure(state->r,state->lr,state->w,state->lw,compf);
  
  PRINTF(" trials %d %e --",j,res[0]);
    
  

  for (i=0; i<j; i++){ 
    COPYSP(*ts,state->s->r->ts->data[i]);
    COPYSP(*ws,state->s->w->ts->data[i]);
    COPYSP(state->s->r->ts->data[i],state->s->w->ts->data[j-i-1]);
    COPYSP(state->s->w->ts->data[i],state->s->r->ts->data[j-i-1]);
    COPYSP(state->s->r->ts->data[j-i-1],*ws);
    COPYSP(state->s->w->ts->data[j-i-1],*ts);
    res[h++] = normalized_compression_difference_measure(state->r,state->lr,state->w,state->lw,compf);
      
  }




  // putting back the series 

  for (i=0; i<j; i++){ 
    COPYSP(*ts,state->s->r->ts->data[i]);
    COPYSP(*ws,state->s->w->ts->data[i]);
    COPYSP(state->s->r->ts->data[i],state->s->w->ts->data[j-i-1]);
    COPYSP(state->s->w->ts->data[i],state->s->r->ts->data[j-i-1]);
    COPYSP(state->s->r->ts->data[j-i-1],*ws);
    COPYSP(state->s->w->ts->data[j-i-1],*ts);
  }

  
  SORT_ARRAY(res,h);
  
  // removing ties
  j=0;
  count[j] = 1;
  for (i=1; i<h; i++) {   
    if (res[j] != res[i]) { 
      j++;
      res[j] = res[i];
      count[j] = count[j-1] +1;

    } else {
      count[j]++;
    }
  }

  tot = h;
  h = j;
  
  if (debug) { 
    for (i=0;i<h;i++) {
      if (res[i]== reference ) { 
	PRINTF(" (## v %e c %d ##)",res[i],count[i]);
      }
      else 
      PRINTF(" v %e c %d",res[i],count[i]);
    }
    PRINTF("\n");
  }
  
  j = COL-1;
  for (i=0;i<h && j>=0 ;i++) { 
    
    if (tot*PVAL[j]>(double)(tot -count[i])) {
      state->boundaries[j] = res[i];
      if (debug==2) {PRINTF("B %e PV %e I %d \n",state->boundaries[j],PVAL[j],count[i]);}
      j--;
    

    }
  }
  
  // in case there are missing boundaries 
  for (;j>=0;j--) { 
    state->boundaries[j] = res[h-1];
    if (debug) {PRINTF("Extra B %e PV %e I %d \n",state->boundaries[j],PVAL[j],count[h-1]);}
  }
  FREE_SP(ts);
  FREE_SP(ws);
  free(res);
  free(count);
  return 0;

}



int compressionDistance(TimeSeries *stream,
			KolmogorovState **state,
			TimeSeries **val,TimeSeries **pval,
			int N, int M, int DF, int BM) { 

  KolmogorovState *s = (state)?(*state):0;
  Window *rext, *wext; 
  Mat res;
  int i;
  TimeSeries *v = *val;
  TimeSeries *pv = *pval;
  

  if (!state)  { 
   
    if (debug) { 
      PRINTF("State pointer is null ");
    }
    return 0;

  }

  if (!s || !s->s ||   
      (s->s->r && s->s->r->ts && N != s->s->r->ts->max )  || 
      (s->s->w && s->s->w->ts && M != s->s->w->ts->max)) {
    if (debug) { 
      PRINTF("State pointer must be update  N %d M %d DF %d. This is the old state: \n",
	     N,M,DF);
      PRINTKOLSTATE(s,0);
    }
    FREE_KOL_STATE(s);
  } 

  s = updatekolstate(s,stream,N,M,BM);
  // UPDATEKOLSTATE(s,stream,N,M);
  *state = s;
  if (debug) { 
    PRINTF("New state: printing \n");
    PRINTKOLSTATE(s,0);
    PRINTF("New state done  \n");
  }


  // Running window is larger than 0
  if (s->s->w->ts->length>0) { 
    
    if (!s->baseline && s->s->w->ts->length == s->s->r->ts->length && s->BMAX >0) { 
      PRINTF("Boot strap  \n");
      bootstrap_compression(s,zlib_compress);
      //
      s->baseline = 1;

    } 
    
    // The extended window share data with the original windo please
   
 

    res = normalized_compression_difference_measure(s->r,s->lr,s->w,s->lw,zlib_compress);
    if (debug)  { PRINTF("RES %f  \n", res ); }



    ALLOCATETS(v,1,1);
    ALLOCATETS(pv,1,1);
    
    *val = v;
    *pval= pv;

    
    v->data[0].y[0]  = (Mat) res;

    if (s->baseline) { 
      i= 1;
      if (res<s->boundaries[0]) { 
	pv->data[0].y[0] = 0; 
	if (debug) PRINTF("### B %e res %e I %d \n",s->boundaries[0],res,0);
	i = COL;
      }
      
      for (;i< COL;i++) {
	if (res <= s->boundaries[i]) {
	  pv->data[0].y[0] = (Mat) PVAL[i];
	  if (debug) PRINTF("### B %e res %e I %d \n",s->boundaries[i],res,i);
	  i = COL;
	}
      }
      if (i==COL && res > s->boundaries[COL -1 ]) 
	pv->data[0].y[0] = 1;
    }
    else { 

      pv->data[0].y[0] = (Mat) 1 - res;
    }



    v->length= pv->length=1;
    v->max= pv->max=1;
    v->data[0].x = pv->data[0].x  = s->s->w->ts->data[0].x;


    if (debug) PRINTF("-> res %e pval  %e x  %ld \n",res,pv->data[0].y[0],pv->data[0].x);

    return 1;
  } else { 
    if(debug) { 
      PRINTF("Nothing to do \n");

    }
    return 0;
  }



}

