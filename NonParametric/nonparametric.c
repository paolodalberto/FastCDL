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
#define NONPARAMETRIC_MODULE 1

#define IDENT(N) {int j; for (j=0;j<N;j++) {printf(" ");}}

#include <interface.h>
#include <nonparametric.h>
#include <pvalue.h>


static int debug =0;
 
char *PrintNONPState(NonParametricState *t, int identation, int P) { 
  char *temp=0;

  if (t) {  
    char *line;
    if (t->r) { 
      int len = t->r->ts->max*256*t->r->ts->dimensions+identation+128;
      
      line  = (char *) calloc(4*len,sizeof(char));
      temp  = (char *) calloc(32*len,sizeof(char));
    }
    else { 
      line  = (char *) calloc((64+identation+128),sizeof(char));
      temp  = (char *) calloc((128+3*identation+128),sizeof(char));
    }
    assert(line && temp);
    IDENTS(identation,temp,line);SPRINT(temp,line,"SNONP B %u \n",t->sizeinbytes);
    IDENTS(identation,temp,line);SPRINT(temp,line,"R storage %d:\n",(t->r)?t->r->ts->max:0);
    if (t->r) {
      char *ts = PrintWindow(t->r,identation+3,0);
      if (ts) { 
	strcat(temp,ts);
	free(ts); // because I need to deallocate the temporary string 
      }
    }
    IDENTS(identation,temp,line);SPRINT(temp,line,"W storage %d:\n",(t->w)?t->w->ts->max:0);
    if (t->w)  { 
      char *ts = PrintWindow(t->w,identation+3,0);
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


NonParametricState *
ReadNONPState(int identation, char **temp) { 
  char *running=*temp;
  NonParametricState *t = (NonParametricState *) calloc(1,sizeof(NonParametricState));

  if (running) {  
    int n;
    int data;
    
    running += identation;    
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"SNONP B %u ",&(t->sizeinbytes));
    running += n+1+identation;

    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"R storage %d:",&data);
    running +=n+1;
    if (data) {
      t->r = ReadWindow(identation+3,&running);
    }
    running += identation;    
    FINDSLASHN(running,n);
    running[n] = 0;
    sscanf(running,"W storage %d:",&data);
    running +=n+1;
    if (data)  { 
      t->w = ReadWindow(identation+3,&running);
    }
    t->code = 1;
    //*temp = running;
  }
  return t;

}








int nonParametricDistance(TimeSeries *stream,
			  NonParametricState **state,
			  TimeSeries **val,TimeSeries **pval,
			  int N, int M, int DF, 
			  int type_order) { 

  NonParametricState *s = (state)?(*state):0;
  Window *rext, *wext; 
  Mat *res=0;
  Mat *res2=0;
  TimeSeries *v = *val;
  TimeSeries *pv = *pval;
  
  
  if (!state)  { 
   

    if (debug) { 
      PRINTF("State pointer is null ");
    }
    return 0;

  }

  if (!s || 
      N != s->r->ts->max || 
      M != s->w->ts->max) {
    if (debug) { 
      PRINTF("State pointer must be update N %d M %d DF %d. This is the old state: \n",
	     N,M,DF);
      PRINTNONPSTATE(s,0);
    }
    FREE_NONP_STATE(s);
  } 

  UPDATESTATE(s,stream,N,M);
  *state = s;
  if (debug) { 
    PRINTF("New state: \n");
    PRINTNONPSTATE(s,0);
  }


  // Running window is larger than 0
  if (s->w->ts->length>0) { 

    rext = createExtendedStatistics(s->r->ts, s->w->ts,type_order);


#ifdef PDFONLY
    { 
      Mat *cdf_e, *cdf_e2;

      if (debug) PRINTF("PDF only bin %d", rext->sw->statistics_len);


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
          
      if (debug) PRINTF("CDF topological order bin %d", rext->sw->statistics_len_df);


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

    if (debug) PRINTF("CDF classic");
    
    res = DFMeasure(rext->sw->df,  
		    rext->sw->ext_df,
		    rext->sw->statistics_len_df,
		    res2);
    

#endif    




    ALLOCATETS(v,1,N_METHODS);
    ALLOCATETS(pv,1,N_METHODS);
    
    *val = v;
    *pval= pv;
    
    memcpy(v->data[0].y ,res          ,N_METHODS*sizeof(Mat));
    memcpy(pv->data[0].y,res+N_METHODS,N_METHODS*sizeof(Mat));

    v->length= pv->length= 1;
    v->data[0].x = pv->data[0].x  = s->w->ts->data[0].x;
    
    free(res);
    free(res2);


    if (debug) { 
      //PRINTF(" Extended statistics \n");
      //PRINTWINDOW(rext,0);
      //SetPrint(rext->poset[rext->posetlen-1]);
      //PrintPoAll(rext->poset[0],rext->posetlen);
      //PrintPoAll(rext->poset[rext->posetlen-1]);
    }



    FREEW(rext);

    return 1;
  } else { 
    if(debug) { 
      PRINTF("Nothing to do \n");

    }
    return 0;
  }
  
  
  
  
}
