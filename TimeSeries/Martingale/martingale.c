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
#define MARTINGALE_MODULE

#include<pvalue.h>
#include<martingale.h>
#include <interface.h>
#include <math.h>
#include <window.h>
#include <nonparametric.h>

//#define TOPOLOGICALONLY

static int debug =0;

double power_martingale(double pval, double m, double par);

double     _prev_martingale[N_MARTINGALES] = INITIALIZE_M;
Transducer _fs[N_MARTINGALES] = INITIALIZE_TRANSDUCER;


char *PrintMartingaleState(MartingaleState *t, int identation, int P) { 
  char *temp=0;
  int i;
  if (t) {  
    char *line;
    if (t->w || t->unif_reference) { 
      line  = (char *) calloc((t->max*128*t->w->ts->dimensions+identation+128),sizeof(char));
      temp  = (char *) calloc((14*t->max*128*t->w->ts->dimensions+3*identation+128),sizeof(char));
    }
    else { 
      line  = (char *) calloc((64+identation+128),sizeof(char));
      temp  = (char *) calloc((128+3*identation+128),sizeof(char));
    }
    assert(line && temp);
    IDENTS(identation,temp,line);SPRINT(temp,line,"SMARP MAX %u \n",t->max);
    IDENTS(identation,temp,line);
    SPRINT(temp,line,"EPS %e ",	   t->epsilon);
    SPRINT(temp,line,"T %e ",	   t->t);
    SPRINT(temp,line,"L %e \n",	   t->lambda);
    for (i=0;i<N_MARTINGALES;i++) { 
      IDENTS(identation,temp,line);
      SPRINT(temp,line,"PREV %e ", t->prev_martingale[i]);
    }
    SPRINT(temp,line,"%s", "\n");
    if (t->w) {
      char *ts = PrintWindow(t->w,identation+3,0);
      if (ts) { 
	strcat(temp,ts);
	free(ts); // because I need to deallocate the temporary string 
      }
    }
    if (t->pval)  { 
      char *ts = PrintWindow(t->pval,identation+3,0);
      if (ts) { 
	strcat(temp,ts);
	free(ts); // because I need to deallocate the temporary string 
      }
    }
    if (t->unif_reference)  { 
      char *ts = PrintWindow(t->unif_reference,identation+3,0);
      if (ts) { 
	strcat(temp,ts);
	free(ts); // because I need to deallocate the temporary string 
      }
    }
    free(line);
    if (P==1) PRINTF("%s\n",temp);
  }
  return temp;

}


MartingaleState *
ReadMARPState(int identation, char **temp) { 
  char *running=*temp;
  MartingaleState *t;
  int i;

  ALLOCATE_MARTINGALE_STATE(t);

  if (running) {  
    int n;
    int data;
    if (debug) {PRINTF("%s\n", running); }
    running += identation;    
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"SMARP B %d ",&(t->max));
    running +=n+1;   
    FINDSLASHN(running,n);
    running[n] = 0;
    if (debug) {PRINTF("%s-\n", running);}
    sscanf(running,"EPS %lf T %lf L %lf ",
	   &t->epsilon,
	   &t->t,
	   &t->lambda);

    running +=n+1;   
    FINDSLASHN(running,n);
    running[n] = 0;
    if (debug) {PRINTF("%s-\n", running);}
    for (i=0;i<N_MARTINGALES;i++) { 
      int j=0;
      sscanf(running,"PREV %lf ", &(t->prev_martingale[i]));
      FINDCHAR(running,' ',j); running += j+1;
      FINDCHAR(running,' ',j); running += j+1;
    }

    running += 1;
    if (debug) {PRINTF("%s-\n", running); }
    t->w = ReadWindow(identation+3,&running);    
   
    if (debug) {PRINTF("P %s-\n", running); }
    
    t->pval = ReadWindow(identation+3,&running);
    if (debug) {PRINTF("U %s-\n", running);}

    t->unif_reference = ReadWindow(identation+3,&running);

    
    
    *temp = running;
  }
  return t;

}

// sorted decreasing decreasing 
int binary_search_sorted_pdf(Window *w, SinglePoint *sp) { 
  int found = 0;
  int i   = w->sw->statistics_len/2;
  int len = w->sw->statistics_len;
  int left = 0;
  double *array = w->sw->pdf;
  double pdf = sp->pdf;

  if (debug) { PRINTTIMESERIES(w->sw->sortedts,0); PRINTSP(*sp);}
  
  while (!found) { 

    if (debug)  { 
      PRINTF(" length=%d and i = %d\n pdf = %lf", len,i,array[i]);  
      
    }
   
    if (len ==0) 
      break;
    
    if (pdf==array[i] || (i>0 &&  pdf< array[i-1] && pdf>array[i])  )
      found = 1;
    else {
      if (pdf>array[i]) {
	len = len/2;
	i = left + len/2;
	
      } else { 
	len =  left+len-1-i;
	left = i+1;
	//	len = (len%2)?(len/2+1):(len/2);
	i = left + len/2;
	
      } 
    }
    if ( (len == 0) || i >= w->sw->statistics_len) { 
      i=-1;
      found =1;
    }
  } 

  if (found) return i;
  else return -1;
  
}





double stochastic_distance_strangeness_pvalue_statistical_window(Window *w, SinglePoint *sp) { 
  
  int i = binary_search_sorted_pdf(w,sp);
  if (i>=0 && i<w->sw->statistics_len && w->sw->statistics_len>1) {
    double f=0;
    int j=0;
    for (j=0; j<=i;j++) { 
      f += 1.0/ w->sw->statistics_len;
    }


    return f;
  } 
  else  { 
    
    if (debug) PRINTF(" not in the set  %ld\n",sp->x);
    
    return 1; 
  }
    
}



Window * updateWindowBySinglePoint(Window *w,  SinglePoint *sp,int max) { 
  TimeSeries *t;
  
  ALLOCATETS(t,1,sp->dimensions);
  COPYSP(t->data[0],*sp);
  t->length++;
  w = WindowUpdate(w,t,max);
  
  FREETS(t);
 
  return w;
}





double stochastic_distance_strangeness_pvalue(TimeSeries *ts, SinglePoint *sp, void *state) {
  double temp;
  TimeSeries *s;
  Window *w;
  int pos = 0;

  if (debug) {
    PRINTF(" PVALUE strangeness \n");
#ifdef RDEF
  R_FlushConsole(); 
#endif


  }

  
  UPDATETS(s,ts,sp);

  

  w = ((MartingaleState *)state)->w;
  if (debug) { 
    PRINTWINDOW(w,0);
  }


  w = WindowUpdate(w,s,((MartingaleState *)state)->max);
  ((MartingaleState *)state)->w = w;
  if (debug) { 
    PRINTWINDOW(w,0);
  }


  FREETS(s);
  if (w->ts->length< ((MartingaleState *)state)->max) 
    return -1;
  
  w = createStatistics(w->ts,POSET); 
  

  if (debug) { 
    PRINTWINDOW(w,0);
  }
  
  for (pos =0; pos < w->sw->sortedts->length; pos++ )
    if (sp->x == w->sw->sortedts->data[pos].x) {
      sp = &(w->sw->sortedts->data[pos]);
      break;
    }

  temp = stochastic_distance_strangeness_pvalue_statistical_window(w,sp);
  
  FREEW(w);
  return  temp;
    
}



static 
double * uniform_range_statistics_comparison (MartingaleState *s) { 
  Window *rext, *wext;
  Mat *res=0;
  double *result=0;
  int i;
  
  for (i=0; i<s->pval->ts->length;i++) {
    s->pval->ts->data[i].code =1;

  }
  

  rext = createExtendedStatistics(s->pval->ts, s->unif_reference->ts,POSET);

#ifdef PDFONLY
    { 
      Mat *cdf_e, *cdf_e2;

      PRINTF("PDF only");


      cdf_e  = DistributionFunctionFromHistogram(rext->sw->pdf,    rext->sw->statistics_len,1);
      cdf_e2 = DistributionFunctionFromHistogram(rext->sw->ext_pdf,rext->sw->statistics_len,1);
      
      res = DFMeasure(cdf_e,  
		      cdf_e2,
		      rext->sw->statistics_len,
		      res);
      
      
      free(cdf_e);
      free(cdf_e2);
    }
#endif
#ifdef TOPOLOGICALONLY
    {
      Mat *cdf_e, *cdf_e2;
          
      PRINTF("CDF topological order %ld \n",s->pval->ts->data[0].x);


      cdf_e  = DistributionFunctionFromHistogram(rext->sw->df,    rext->sw->statistics_len_df,1);
      cdf_e2 = DistributionFunctionFromHistogram(rext->sw->ext_df,rext->sw->statistics_len_df,1);
      
      res = DFMeasure(cdf_e,  
		      cdf_e2,
		      rext->sw->statistics_len_df,
		      res);
      
      
      free(cdf_e);
      free(cdf_e2);
   }
#endif

#ifdef CDFCLASSICONLY

    PRINTF("CDF classic");
    
    res = DFMeasure(rext->sw->df,  
		    rext->sw->ext_df,
		    rext->sw->statistics_len_df,
		    res2);
    

#endif    
  
  PRINTRESULTDISTANCE(res,res+N_METHODS);
  result = filtering_quorum_t(res,result,s->fraction);
  
  if (res) free(res);

  FREEW(rext);

  return result;
	
}

static 
double uniform_range_statistics_measure(MartingaleState *s, DistanceFunctionsPvalue dist) { 
  Statistic result; 
  Window *rext, *wext;
  
  rext = createExtendedStatistics(s->pval->ts, s->unif_reference->ts,POSET);


#ifdef PDFONLY
    { 
      Mat *cdf_e, *cdf_e2;

      if (debug) { PRINTF("PDF only"); }


      cdf_e  = DistributionFunctionFromHistogram(rext->sw->pdf,    rext->sw->statistics_len,1);
      cdf_e2 = DistributionFunctionFromHistogram(rext->sw->ext_pdf,rext->sw->statistics_len,1);
      
      result = dist(cdf_e,  
		 cdf_e2,
		 rext->sw->statistics_len
		 );
      
      
      free(cdf_e);
      free(cdf_e2);
    }
#endif
#ifdef TOPOLOGICALONLY
    {
      Mat *cdf_e, *cdf_e2;
          
      if (debug) {       PRINTF("CDF topological order");}


      cdf_e  = DistributionFunctionFromHistogram(rext->sw->df,    rext->sw->statistics_len_df,1);
      cdf_e2 = DistributionFunctionFromHistogram(rext->sw->ext_df,rext->sw->statistics_len_df,1);
      
      result = dist(cdf_e,  
		 cdf_e2,
		 rext->sw->statistics_len_df
		 );
      
      
      free(cdf_e);
      free(cdf_e2);
    }
#endif

#ifdef CDFCLASSICONLY

    if (debug) {  PRINTF("CDF classic");}
    
    result = dist(rext->sw->df,
		  rext->sw->ext_df,
		  rext->sw->statistics_len_df
		  );

    

#endif    

  
  FREEW(rext);
  
  return result.pval;
	
}

/********************
 * How to generate uniform distributions
 *
 */ 

typedef Window* (*Runif)(Window *, int, int );


static 
Window  *build_uniform_distributed_series_0_1_random(Window *ref, int max, int dimensions) { 

  Window *w;
  TimeSeries *rts;
  int i,j;
  unsigned short seed[3];
  ALLOCATETS(rts,max,dimensions);
  
  
  seed[0] = ((unsigned short) ((long int) &(rts->data[0].x))) & 0xff;
  seed[2] = ((unsigned short) ((long int) rts) & 0xff) ;
  seed48(seed);
  for (i=0;i< max;i++) {
    rts->data[i].code =1;
    rts->data[i].x = i+1;
    for (j=0;j<dimensions;j++) 
      rts->data[i].y[j] = drand48(); // unifrm between 0-1 
  }
  rts -> length = max;
    
  w = WindowUpdate(ref,rts,max);
  FREETS(rts);
  return w;
}


static  
Window  *build_uniform_distributed_series_0_1(Window *ref, int max, int dimensions) { 

  Window *w;
  TimeSeries *rts;
  int i,j;
  long double step=0;
  double inc=(1.0/max);
  ALLOCATETS(rts,max,dimensions);
  
  step += inc;
  for (i=0;i<max;i++) {
    rts->data[i].code =1;
    rts->data[i].x = (Xorder) i+1;
    for (j=0;j<dimensions;j++) 
      rts->data[i].y[j] = (Mat) step; // unifrm between 0-1 
    step += inc;
  }
  rts -> length = max;
  w = WindowUpdate(ref,rts,max);
  FREETS(rts);
  return w;
}


/********************
 * Handling the martingales 
 *
 */ 



double power_martingale(double pval, double m, double par) { 
  return (pval>0)?(par * pow(pval,par-1.0) * m):1;

}



static inline
void compute_and_update_martingales(double *ms, double *prevms, 
				    double *pvals, double *par, 
				    MartingaleState *s ) { 
  int i;
  
  for (i=0;i<N_MARTINGALES;i++) { 
    ms[i] = s->fs[i](pvals[i],prevms[i],par[i]);
  }

}


static inline
void overwrite_martingales(double *ms, double *prevms) {
  int i;
  for (i=0;i<N_MARTINGALES;i++) { 
    ms[i] = prevms[i];
  }
  
}

static inline
void set_to_one_martingales(double *ms) {
  int i;
  for (i=0;i<N_MARTINGALES;i++) { 
    ms[i] = 1.0;
  }
  
}

static inline
void state_overwrite_martingales(double *ms, MartingaleState *s) {
  int i;
  for (i=0;i<N_MARTINGALES;i++) { 
    s->prev_martingale[i] = ms[i];
  }

}
static inline
void state_martingales_upload(double *ms, MartingaleState *s) {
  int i;
  for (i=0;i<N_MARTINGALES;i++) { 
    ms[i] = s->prev_martingale[i] ;
  }

}

static 
int logic_martingale(double *ms, double *prevms, double l, double t, int left) { 
  
  int r =  ms[0]>=l || fabs((ms[0]-prevms[0])/prevms[0])>=t;
  
  if (left==1)
    return  r ;
  else 
    return r || (/* (1/ms[0])>l  &&*/ logic_martingale(++ms, ++prevms, l,t, --left));
  
}

static inline 
double maximum (double *m, int l) { 
  double max = m[0];
  int i;
  for (i=1;i<l;i++) 
    if (max<m[i]) 
      max = m[i];
  
  return max;

}


/***************************
 * Conformal prediction martingale body
 *
 *
 */ 


#ifndef MAX
#define MAX(a,b) ((a)>(b))?(a):(b)
#endif

int  conformal_prediction_martingale(TimeSeries *ts, 
				     MartingaleState **state,
				     TimeSeries **val,  // Martingale value
				     TimeSeries **pval, // pvalue 
				     TimeSeries **resp, // responce variation/no 
				     double epsilon, // confidence
				     double t,       // upperbound incremental martingale difference
				     double lambda,  // upperbound martingale 
				     double fraction,
				     int M,          // Window size 
				     Strangeness_pvalue pvalueOfDistanceFunction) { 

  int i;
  SinglePoint *pts;
  double martingales[N_MARTINGALES];
  double martingales_prev[N_MARTINGALES];
  double pvalues[N_MARTINGALES];
  double params[N_MARTINGALES];
  Runif  genUniformSeries = build_uniform_distributed_series_0_1;

  MartingaleState *s = (state)?(*state):0;  
  double *result;
  Mat *res;
  TimeSeries *rts;
  TimeSeries *pv, *ym;
  Window *temp;
  int counter =0;
  int anomaly = 0;

  // initialization of the epsilons 
  for (i=0;i<N_MARTINGALES;i++) 
    params[i] = epsilon;
  
  params[1] = 0.99;

  PRINTF(" \n");
#ifdef RDEF
  R_FlushConsole(); 
#endif

  if (!ts  || M<=0 || epsilon<0 || epsilon >1 || t<=0 || lambda <=0 ) { 
    PRINTF(" !%ld  || %d<=0 || %e<0 || %e >1 || %e<=0 || %e <=0 \n",(long int) ts, M,epsilon, epsilon,t,lambda); 
#ifdef RDEF
  R_FlushConsole(); 
#endif
    return anomaly; 

  } 
  if (debug) { 

    PRINTTIMESERIES(ts,0);
#ifdef RDEF
  R_FlushConsole(); 
#endif
  }
  
  anomaly++;

  if (!s) { 
    { 
      s = (MartingaleState*) calloc(1,sizeof(MartingaleState));         
      s->w = s->pval=s->unif_reference=0;                             
      s->epsilon=s->t =s->lambda =0; s->max = 0;
      s->prev_martingale = (double*) calloc(N_MARTINGALES,sizeof(double));  
      memcpy( s->prev_martingale,_prev_martingale,2*sizeof(double));s->fs = _fs; 
    }
    /* ALLOCATE_MARTINGALE_STATE(s); */
    s->fraction = fraction;
    *state = s;
  }


  if (s->t != t || s->lambda != lambda || s->epsilon != epsilon) {
    s-> t = t;
    s-> lambda = lambda;
    s->epsilon = epsilon; 
  } 
  
  if (M != s-> max) {
    s-> max  = M;
    if (s->pval) {
      temp           = WindowUpdate(0,s->pval->ts,s->max);
      FREEW(s->pval);
      s->pval = temp;
    }
    if (s->w) { 

      temp             = WindowUpdate(0,s->w->ts,s->max); 
      FREEW(s->w);
      s->w = temp;
      temp=0;
    }
    if (!s->unif_reference) {
      
      // s->unif_reference = genUniformSeries(s->unif_reference,s->max,1);

    } else 
      if ( s->unif_reference->ts->length < s->max)
	s->unif_reference = WindowUpdate(s->unif_reference,s->unif_reference->ts,s->max); 
      else 
	FREEW(s->unif_reference)
	  
  }
  /*
  if (!s->unif_reference) { 
    s->unif_reference = genUniformSeries(s->unif_reference,s->max,1);
  } 
  */

  /******
   *  Starting the body of the computatiion
   */ 

  if (debug) { 
    PRINTMARTINGALESTATE(s); 
#ifdef RDEF
  R_FlushConsole(); 
#endif
  } 


  ALLOCATETS(rts,ts->length,1);
  ALLOCATETS(pv ,ts->length,1);
  ALLOCATETS(ym ,ts->length,1);

  rts->length = pv->length = ym->length=ts->length;

  // upload previous martingales
  state_martingales_upload( martingales_prev,s);

  ALLOCATE_SP(pts,1);


  for (i=0; i< ts->length;i++) { 
    double powt,antipowt;
    anomaly = 1;

    pts->x    = ts->data[i].x;

    if (debug) { 
      PRINTTIMESERIES(ts,0);
      PRINTSP(ts->data[i]);PRINTF("\n");
#ifdef RDEF
  R_FlushConsole(); 
#endif
    }
    if (debug) {
      PRINTF(" PVALUE strangeness \n");
#ifdef RDEF
      R_FlushConsole(); 
#endif
    }
    pvalues[0] = pvalueOfDistanceFunction(0,ts->data + i,s);



    if (pvalues[0] < 0) { // initialization phase 

      if (debug) { 
	PRINTF(" Initializing the martingale \n");
      }

    

      overwrite_martingales( martingales, martingales_prev);
      
      
      rts->data[i].x = 0;
      pv->data[i].x  = 0;
      ym->data[i].x  =0;

    } else {
      // because the pvalue >0 we have something into the state
      // so we can use the pvalue of the distance function 
      
      if (debug) { 
	PRINTF("  martingale running\n");
      }

            // we use the real data as reference .... 


      pts->y[0] = pvalues[0];



      s->pval = updateWindowBySinglePoint(s->pval,pts, s->max);
      /*      PRINTSP(*pts);PRINTF("\n");
	      PRINTWINDOW(s->pval,0);
      */

      if (!(s->unif_reference) ||  s->unif_reference->ts->length < s->max) { 

	s->unif_reference =  updateWindowBySinglePoint(s->unif_reference,pts, s->max);
	if (debug) { 
	  PRINTSP(*pts);PRINTF("Unif Reference \n");
	  PRINTWINDOW(s->unif_reference,0);
	  
	}
      }
      
      if (debug) {PRINTF("PVAL Reference \n"); PRINTWINDOW(s->pval,3); } 
      //      pvalues[1]  = 1-uniform_range_statistics_measure(s,CamberraWithPValue);
      pvalues[1]  = 1-pvalues[0];
      if (pvalues[1] <=0) 
	 pvalues[1]= 1;

      compute_and_update_martingales(martingales,martingales_prev,pvalues,params,s);

      if (debug) {PRINTF("Done compute_and_update_martingales \n");} 

      if (logic_martingale(martingales,martingales_prev,lambda,t,N_MARTINGALES)) {
	
	anomaly = anomaly|2;
	if (debug)  {
	  int j;
	  for (j=0;j<N_MARTINGALES;j++) { 
	    PRINTF(" Anomaly found using martingale at %ld M %e M-prev %e  ",
		   ts->data[i].x,martingales[j] ,martingales_prev[j]); 
	  }
	  PRINTF("\n");
	}
	
      } else { 
	
      }
      rts->data[i].x = pts->x;
      pv->data[i].x  = pts->x;
      ym->data[i].x  = pts->x;

      
    }

    rts->data[i].y[0] = maximum(martingales,N_MARTINGALES);
    pv->data[i].y[0]  = pvalues[0];
    ym->data[i].y[0]  = (anomaly>=2)?1:0;
    
    overwrite_martingales( martingales_prev, martingales);


    counter ++ ;
    if (counter > s-> max) { 
      counter =0;
      anomaly = 1;
            
      if (s->pval->ts->length >= s->unif_reference->ts->length) {

	result = uniform_range_statistics_comparison (s);
	
	if (result[1]==1)  { 
	  anomaly = anomaly | 4;
	  
	  if (1) PRINTF(" Anomaly found using the pvalue non uniform at %ld \n", pv->data[i].x);
	  ym->data[i].y[0]  = 1;
	  
	} else { 
	  set_to_one_martingales(martingales_prev);

	}
	free(result);
      }
    }

  }

  state_overwrite_martingales(martingales,s);

  *val = rts;
  *pval = pv;
  *resp = ym;

  if (debug) { PRINTTIMESERIES(rts,0); } 
  SORT_TIMESERIES(rts,TIMESLOT); 
  if (debug) { PRINTTIMESERIES(rts,7); } 
  if (debug) { PRINTTIMESERIES(pv,0); } 
  SORT_TIMESERIES(pv,TIMESLOT);
  if (debug) { PRINTTIMESERIES(pv,7); } 
  if (debug) { PRINTTIMESERIES(ym,0); } 
  SORT_TIMESERIES(ym,TIMESLOT); 
  if (debug) { PRINTTIMESERIES(ym,7); } 

  if (debug) { PRINTF(" Done Sorting \n"); }

  FREE_SP(pts);

  return anomaly;



}


