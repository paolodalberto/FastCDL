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
#define MOVINGAVERAGE_MODULE 1
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <window.h>
#include <movingaverage.h>


static int debug =0;

char * 
PrintMAState(MovingAverageState *t, int identation, int P) { 
  char *temp = 0;

  if (t) { 
    char *line;
    char *sp;
    if (t->x || t->e) {
      CircularBuffer *h = (t->x)?t->x:t->e;
      line = (char *) calloc((h->length*64+identation+128),sizeof(char));
      temp = (char *) calloc((3*h->length*64+3*identation),sizeof(char));

    } else { 
      line = (char *) calloc((64+identation+128),sizeof(char));
      temp = (char *) calloc((3*64+3*identation+128),sizeof(char));

    }
    assert(line&&temp);
    IDENTS(identation,temp,line);SPRINT(temp,line,"SMA  B %u\n",t->sizeinbytes);
    IDENTS(identation,temp,line);
    sprintf(line,"gamma %e\n",t->gamma);
    strcat(temp,line);
    IDENTS(identation,temp,line);
    //sprintf(line,"ot %e et %e\n",t->o_t,t->e_t);
    sp = PrintSP(t->o_t,0,0);
    sprintf(line,"ot %s ",sp);
    strcat(temp,line);    free(sp);
    sp = PrintSP(t->e_t,0,0);
    sprintf(line,"et %s\n",sp);
    strcat(temp,line);free(sp);
    IDENTS(identation,temp,line);SPRINT(temp,line,"X storage %d :\n", (t->x)?t->x->length:0);
    if (t->x) { 
     char *ts = PrintCircular(t->x,identation+3,0);
     strcat(temp,ts);
     free(ts); // because I need to deallocate the temporary string 
    }
    IDENTS(identation,temp,line);SPRINT(temp,line,"D storage %d :\n",(t->e)?t->e->length:0);
    if (t->e) { 
      char *ts = PrintCircular(t->e,identation+3,0);
      strcat(temp,ts);
      free(ts); // because I need to deallocate the temporary string 
    }
    
    free(line);
  }
  else { 
    printf("empty state \n");
  }
  printf("%s",temp);
  return temp;

}



MovingAverageState * 
ReadMAState(int identation, char **temp) { 

  char *running=*temp;
  MovingAverageState *t = (MovingAverageState *) calloc(1,sizeof(MovingAverageState));
  if (running) { 
    int n;
    int data;
    int flag;
    SinglePoint *sp;
    
    running += identation;
    FINDSLASHN(running,n);
    running[n] = 0;
    flag = sscanf(running,"SMA  B %u",&(t->sizeinbytes));
    running += n+1+identation;

    //FINDCHAR(running,' ',n);
    //running += n+1;
    
    //FINDCHAR(running,' ',n);
    //running += n+1;

    FINDSLASHN(running,n);
    running[n]=0;

    flag = sscanf(running,"gamma %lf",&(t->gamma));
    //t->gamma = atof(running);
    
    running += n+1+identation;
    FINDCHAR(running,'[',n);
    running += n;

    if (debug) printf("debug 7 %s\n",running);

    //t->o_t = atof(running);
    t->o_t = readSP(0,&running);
    //COPYSP(*(t->o_t),*sp); FREE_SP(sp);

    //flag = sscanf(running,"%f",&(t->o_t));
    

    FINDCHAR(running,'[',n);
    running += n;

    FINDSLASHN(running,n);
    running[n]= 0;

    //t->e_t = atof(running);
    t->e_t = readSP(0,&running);
    //COPYSP(*(t->e_t),*sp); FREE_SP(sp);
    //    flag = sscanf(running,"%f",&(t->e_t));

    running ++;
    if (debug) printf("debug 13 %s\n",running);
    if (debug) printf("debug 14 %s\n",running);

    FINDSLASHN(running,n);
    running[n]= 0;
    sscanf(running,"X storage %d :",&data);
    running += n+1;
    if (data) { 
     t->x = ReadCircular(identation+3,&running);
    }
    running ++;
    running += identation;
    FINDSLASHN(running,n);
    running[n] = 0;
 
    sscanf(running,"D storage %d :",&data);
    running += n+1;
    if (data) { 
      t->e = ReadCircular(identation+3,&running);
    }
 

    t->code=1;
    //*temp = running;

  }

  return t;
}

/*
  movingAverageSmoothing(stream,
			 ym->y,
			 dm->y,
			 &(s->ot),  &(s->et),
			 s->x,  s->o, s->t,
			 gamma);
*/
int movingAverageSmoothing (TimeSeries  *stream,
			    SinglePoint * ytm,   // time series
			    SinglePoint * dtm,   // time series
			    SinglePoint * yt,    // single point
			    SinglePoint * dt,    // single point
			    CircularBuffer * x,
			    CircularBuffer * e) {

  
  int L = stream->length;
  int i;
  SinglePoint ot = *yt;
  SinglePoint et = *dt;
  SinglePoint *new,*last;

  ALLOCATE_SP(new,stream->dimensions);
  ALLOCATE_SP(last,stream->dimensions);
  

  if (debug) { 
    printf("movingAverageSmoothing P %d \n",x->length);
    

  }
 
  for (i=0;i<L;i++) { 
    et.x= ot.x = stream ->data[i].x;
      
    //ot += (stream ->y[i]-BOTTOM(x))/x->length;
    ot = SP_add(&ot, ot, 
		SP_mul(new, 1/x->length, 
		       SP_sub(new,stream ->data[i],BOTTOM(x))));

    // last = |BOTTOM(x)-BOTTOM(e)|/x->length;
    *last = SP_mul(last,1/x->length,
		   SP_abs(last,
			  SP_sub(last,BOTTOM(x),BOTTOM(e))));

    //new  = |stream ->y[i]-ot|/x->length;
    *new = SP_mul(new,1/x->length,
		  SP_abs(new,
			 SP_sub(new,stream->data[i],ot)));

    //et = et + new - last;
    et = SP_add(&et,et,SP_sub(&et,*new,*last));

    COPYSP(ytm[i], ot);
    //ytm[i] = stream->y[i];
    COPYSP(dtm[i],et);

    SHIFTADD(x,stream ->data[i]);
    SHIFTADD(e,ot);
    
  }
  
  *yt=ot;
  *dt=et; 

  FREE_SP(new );
  FREE_SP(last);


  return 1;
}




int MovingAverage(TimeSeries *stream,
		  MovingAverageState **state,
		  TimeSeries **ytmOut,
		  TimeSeries **dtmOut,
		  int P
		  ) { 

  MovingAverageState *s = (MovingAverageState *)((state)?(*state):0);  
  TimeSeries *ym, *dm; 
  


  if (debug) { 
    printf("Moving Average \n");
    PRINTTIMESERIES(stream,0); 
    printf("\n");  
    
  }

  if (!state) { 
    if (debug) printf("State pointer is null ");
    
    return 0;
  }

  
  if (!s || (P && !s->x)  || 
      (s->x && P != s->x->length) ) { 
    
    
    if (debug) { 
      printf("State pointer must be update. This is the old state: \n");
      PRINTMASTATE(s,0);
    }

    
    FREE_MA_STATE(s);
    ALLOCATE_MA_STATE(s,P,stream->dimensions);
    *state = s;

    if (debug) { 
      printf("This is the new state: \n");
      PRINTMASTATE(s,0);
    }
    
  } 
  else  
    {
      
      if (debug) { 
	printf("State is here: \n");
	PRINTMASTATE(s,0);
      }
    }
  

  ALLOCATETS(ym,stream->length+1,stream->dimensions);
  ALLOCATETS(dm,stream->length,stream->dimensions);
  *ytmOut = ym;
  *dtmOut = dm;

  COPY_TS(ym, stream);
  COPY_TS(dm, stream);
  ym->length = stream->length+1;
  dm->length = stream->length;
  
  COPYSP(ym->data[0],*(s->o_t));
  

  movingAverageSmoothing(stream,
			 ym->data,
			 dm->data,
			 s->o_t,  s->e_t,
			 s->x,  s->e);

  if (debug) { 
    printf("Output Yt:");
    PRINTTIMESERIES(ym,3);
    printf("Output Dt:");
    PRINTTIMESERIES(dm,3);
  }
  return 1;



}
  
