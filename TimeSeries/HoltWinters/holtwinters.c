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
#define HOLTWINTERS_MODULE 1
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <window.h>
#include <holtwinters.h>


//#define IDENT(N) {int j; for (j=0;j<N;j++) {printf(" ");}}

static int debug =0;

char * 
PrintHWState(HoltWinterState *t, int identation, int P) { 
  char *temp=0;


  if (t) {  
    char *line; 
    char *sp;
    if (t->c || t->d ) { 
      CircularBuffer *h = (t->c)?t->c:((t->d)?t->d:0);
      //printf("h->length %d ",h->length);
      //printf("h->array[0].dimensions %d ",h->array[0].dimensions);
      line = (char *) calloc((h->length*64*h->array[0].dimensions+identation+128),sizeof(char));
      temp = (char *) calloc((4*h->length*64*h->array[0].dimensions+3*identation),sizeof(char));
    } else { 
      line = (char *) calloc((5*64*t->at->dimensions+128+3*identation),sizeof(char));
      temp = (char *) calloc((10*64*t->at->dimensions+3*identation+128),sizeof(char));
    }
    assert(line&&temp);
    IDENTS(identation,temp,line);SPRINT(temp,line,"SHW  B %u\n",t->sizeinbytes);
    IDENTS(identation,temp,line);
    sprintf(line,"alpha %e beta %e gamma %e\n",t->alpha,t->beta,t->gamma);
    strcat(temp,line);
    IDENTS(identation,temp,line);
    sp = PrintSP(t->yt,0,0);
    sprintf(line,"yt %s ",sp);
    strcat(temp,line);    free(sp);
    sp = PrintSP(t->at,0,0);
    sprintf(line,"at %s ",sp);
    strcat(temp,line);free(sp);
    sp = PrintSP(t->bt,0,0);
    sprintf(line,"bt %s ",sp);
    strcat(temp,line);free(sp);
    sp = PrintSP(t->dt,0,0);
    sprintf(line,"dt %s\n",sp);
    strcat(temp,line);free(sp);
    //    sprintf(line,"yt %e at %e bt %e dt %e\n",t->yt,t->at,t->bt,t->dt);

    IDENTS(identation,temp,line);SPRINT(temp,line,"C storage %d :\n", (t->c)?t->c->length:0);
    if (t->c) { 
     char *ts = PrintCircular(t->c,identation+3,0);
     strcat(temp,ts);
     free(ts); // because I need to deallocate the temporary string 
    }
    IDENTS(identation,temp,line);SPRINT(temp,line,"D storage %d :\n",(t->c)?t->d->length:0);
    if (t->d) { 
      char *ts = PrintCircular(t->d,identation+3,0);
      strcat(temp,ts);
      free(ts); // because I need to deallocate the temporary string 

    }
    
    if (P==1) printf("%s",temp);

    free(line);
 }
  return temp;
}

HoltWinterState * 
ReadHWState(int identation, char **temp) { 
  
  char *running=*temp;

  HoltWinterState *t = (HoltWinterState *) calloc(1,sizeof(HoltWinterState));
  if (running) {  
    int n;
    int data;
    SinglePoint *sp;
    running += identation;
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"SHW  B %u",&(t->sizeinbytes));

    running += n+1+identation;
    FINDCHAR(running,' ',n);
    running += n+1;
    FINDCHAR(running,' ',n);
    running[n]=0;
    t->alpha = atof(running);
    running += n+1;
    FINDCHAR(running,' ',n);
    running += n+1;
    FINDCHAR(running,' ',n);
    running[n]=0;
    t->beta = atof(running);
    running += n+1;
    FINDCHAR(running,' ',n);
    running += n+1;
    FINDSLASHN(running,n);
    running[n]=0;
    t->gamma = atof(running);
    
    running += n+1+identation;
    FINDCHAR(running,' ',n);
    running += n+1;
    t->yt = readSP(0,&running);
    /*COPYSP(*(t->yt),*sp); 
      FREE_SP(sp); */
    FINDCHAR(running,'[',n);
    running += n;
    t->at = readSP(0,&running);
    /* COPYSP(*(t->at),*sp); 
       FREE_SP(sp); */
    FINDCHAR(running,'[',n);
    running += n;
    t->bt = readSP(0,&running);
    /*COPYSP(*(t->bt),*sp); FREE_SP(sp); */
    FINDCHAR(running,'[',n);
    running += n; 
    t->dt = readSP(0,&running);
    /* COPYSP(*(t->dt),*sp); FREE_SP(sp);*/

    running ++;
    FINDSLASHN(running,n);
    running[n] = 0;
    
    sscanf(running,"C storage %d :",&data);
    running += n+1;
    if (data) { 
     t->c = ReadCircular(identation+3,&running);
    }

    running ++;
    running += identation;
    FINDSLASHN(running,n);
    running[n] = 0;
 
    sscanf(running,"D storage %d :",&data);
    running += n+1;
    if (data) { 
      t->d = ReadCircular(identation+3,&running);
    }
    t->code = 1;
    //*temp = running;
  }
  return t;
}
/*
void FREE_HW_STATE(HoltWinterState *f) {
  if (f) {				
    if (f->code) {                    
      FREECIRCULAR((f)->d);		
      FREECIRCULAR((f)->c); }		
      free(f); (f)=0;			
  }					
} 
*/




HoltWinterState* allocate_hw_state(int N, double a,double b,double g,unsigned int k) {
    
  HoltWinterState *f = (HoltWinterState *) calloc(1,sizeof(HoltWinterState));		
  assert(f);
  f->code =1;
  ALLOCATE_SP(f->yt,k); 
  ALLOCATE_SP(f->at,k);                     
  ALLOCATE_SP(f->bt,k); 
  ALLOCATE_SP(f->dt,k);                     
  if (N>0) { ALLOCATECIRCULAR(f->c,N,k);} else f->c=0;		
  if (N>0) { ALLOCATECIRCULAR(f->d,N,k);} else f->d=0;		
  //f->yt= f->at = f->bt = 0; f->code=1; f->dt=0;		
  f->alpha =a; f->beta=b; f->gamma=g;				
    
  return f;
} 


int cyclicExponentialSmoothing (TimeSeries *stream,
				SinglePoint  * ytm, // time series 
				SinglePoint  * dtm, // time series 
				SinglePoint  * yt,  // single point
				SinglePoint  * at,  // single point
				SinglePoint  * bt,  // single point
				SinglePoint  * dt,  // single point
				CircularBuffer * c, 
				CircularBuffer * d,
				Mat alpha,
				Mat beta,
				Mat gamma) {

  
  int L = stream->length;
  SinglePoint *ct,*at_1,*temp1, *temp2;
  int i;

  ALLOCATE_SP(ct,yt->dimensions); 
  ALLOCATE_SP(at_1,yt->dimensions); 
  ALLOCATE_SP(temp1,yt->dimensions); 
  ALLOCATE_SP(temp2,yt->dimensions); 
  
  
  if (debug) { 
    PRINTF("cyclicExponentialSmoothing P %d a %f b %f g %f \n",(c)?c->length:0,alpha,beta,gamma);
  }
 
  for (i=0;i<L;i++) {
    // keep a time stamp 
    ct->x = at_1->x = temp1->x = temp2->x = stream->data[i].x;

    COPYSP(*at_1, *at);
    if (c) {
      COPYSP(*ct, BOTTOM(c));
      ct->x = stream->data[i].x;
      // *at = alpha*(stream ->y[i]-ct)+(1-alpha)*(at_1+*bt);
      if (debug) { 
	PRINTSP(*temp1);
	PRINTSP(*ct);
	PRINTSP(stream->data[i]);
	PRINTF("\n");
      }

      *temp1 = SP_mul(temp1,alpha,SP_sub(temp1,stream->data[i],*ct));

      if (debug) { 
	PRINTSP(*temp1);
	PRINTF("\n");
	PRINTSP(*temp2);
	PRINTSP(*at_1);
	PRINTSP(*bt);
	PRINTF("\n");
      }
      *temp2 = SP_mul(temp2, (1-alpha), SP_add(temp2,*at_1,*bt));
      *at = SP_add(at,*temp1,*temp2);
      if (debug) { 
	PRINTSP(*temp1);
	PRINTSP(*temp2);
	PRINTSP(*at);
	PRINTF("\n");
      }
      //      ct = gamma*(stream->data[i]-*at)+(1-gamma)*ct;
      *temp1 = SP_mul(temp1,gamma,SP_sub(temp1,stream->data[i],*at));
      *temp2 = SP_mul(temp2, (1-gamma), *bt);
      *ct = SP_add(ct,*temp1,*temp2);

      SHIFTADD(c,*ct);
    }
    else { 
      //*at = alpha*(stream->data[i])+(1-alpha)*(at_1+*bt);
      
      *temp1 = SP_mul(temp1,alpha,stream->data[i]);
      *temp2 = SP_mul(temp2, (1-alpha), SP_add(temp2,*at_1,*bt));
      *at = SP_add(at,*temp1,*temp2);
    }
    
    // *bt = beta*(*at - at_1)+ (1-beta)*(*bt);
    *temp1 = SP_mul(temp1,beta,SP_sub(temp1,*at,*at_1));
    *temp2 = SP_mul(temp2, (1-beta), *bt);
    *bt = SP_add(bt,*temp1,*temp2);
     
    
    //ytm[i+1] = *at+ *bt +((c)?(ACCESSFROMBOTTOM(c,1)):0);
    ytm[i+1] = SP_add(ytm+i+1,*at,*bt);
    if (c) { 
      ytm[i+1] = SP_add(ytm+i+1, ytm[i+1], ACCESSFROMBOTTOM(c,1));
    } 

    if (d) {

      COPYSP(dtm[i], BOTTOM(d));
      //*dt = gamma*fabs(stream->data[i] - ytm[i])+(1-gamma)*(BOTTOM(d));
      *temp1 = SP_mul(temp1,gamma,SP_abs(temp1,SP_sub(temp1,stream->data[i],ytm[i])));
      *temp2 = SP_mul(temp2, (1-gamma), (BOTTOM(d)));
      *dt = SP_add(dt,*temp1,*temp2);
		      
      SHIFTADD(d,*dt);

    }
    else { 
      
      // *dt = gamma*fabs(stream->data[i] - ytm[i])+(1-gamma)*(*dt);
      *temp1 = SP_mul(temp1,gamma,SP_abs(temp1,SP_sub(temp1,stream->data[i],ytm[i])));
      *temp2 = SP_mul(temp2, (1-gamma), *dt);
      *dt = SP_add(dt,*temp1,*temp2);
      
      COPYSP(dtm[i],*dt);
    }
  }
  
  COPYSP(*yt, ytm[i]);
  
  FREE_SP(ct); 
  FREE_SP(at_1); 
  FREE_SP(temp1); 
  FREE_SP(temp2); 
  
  return 1;
}


int HoltWinters(TimeSeries *stream,
		HoltWinterState **state,
		TimeSeries **ytmOut,
		TimeSeries **dtmOut,
		int P, 
		Mat alpha,
		Mat beta,
		Mat gamma) { 
  
  
  HoltWinterState *s = (HoltWinterState *)((state)?(*state):0);
  TimeSeries *ym, *dm;

 

  if (debug) { 
    PRINTF("HoltWinters P %d a %f b %f g %f \n",P,alpha,beta,gamma);
    PRINTTIMESERIES(stream,0);
    PRINTF("\n");
#ifdef RDEF
     R_FlushConsole();         
#endif
  }

  if (!state) { 
   
    if (debug) { 
      PRINTF("State pointer is null ");
    }
    return 0;

  }
  if (!s || (P && !s->c)  || 
      (s->c && P != s->c->length) || 
      (alpha != s->alpha) || 
      (beta  != s->beta)    || 
      (gamma != s->gamma)) { 
    
    
    if (debug) {
      PRINTF("State pointer must be update. This is the old state: \n");
      PRINTHWSTATE(s,0);
    }

    
    FREE_HW_STATE(s);
    if (debug) {
      PRINTF("Freed State \n");

    }
    
    s = allocate_hw_state(P,alpha,beta,gamma, stream->dimensions);
    
    /*
      ALLOCATE_HW_STATE(s,P,alpha,beta,gamma, stream->dimensions);
    */
    *state = s;

    if (debug) { 
      PRINTF("This is the new state: \n");
      PRINTHWSTATE(s,0);
    }


  } else {

    if (debug) { 
      PRINTF("State is here: \n");
      PRINTHWSTATE(s,0);
    }
  }


  ALLOCATETS(ym,stream->length+1, stream->dimensions);
  if (debug) { 
    PRINTF("This is the new state: \n");
    PRINTTIMESERIES(ym,0);
  }
  ALLOCATETS(dm,stream->length  , stream->dimensions);
  *ytmOut = ym;
  *dtmOut = dm;

  if (debug) { 
    PRINTF("This is the new state: \n");
    PRINTTIMESERIES(dm,0);

  }


  
  copy_ts(ym,stream);
  copy_ts(dm,stream);
  ym->length = stream->length+1;
  dm->length = stream->length;
  
  COPYSP(ym->data[0], *(s->yt));

  if (debug) PRINTF(" Before cyclicExponentialSmoothing \n");
  
  cyclicExponentialSmoothing(stream,ym->data,dm->data,
			     s->yt, s->at, s->bt, s->dt,
			     s->c, s->d, 
			     alpha,beta,gamma);

  if (debug) { 
    PRINTF("Output Yt:");
    PRINTTIMESERIES(ym,3);
    PRINTF("Output Dt:");
    PRINTTIMESERIES(dm,3);
  }
  return 1;
} 

		
