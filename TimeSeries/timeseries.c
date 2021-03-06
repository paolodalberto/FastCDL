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
#define TIMESERIES_MODULE 1
#define GETTIME
#include <timeseries.h>
#include <pvalue.h>
#include <math.h>


#include <type.h>

static int debug=0; 

#define N_PVALUES_METHODS 3

static Strangeness_pvalue global_array[N_PVALUES_METHODS] = 
  { 
    stochastic_distance_strangeness_pvalue,  // Distributio function based pvalue
    min_cluster_distance_pvalue,             // Nearest Neighbour based pvalue  
    pdfbased_distance_pvalue                 // fast estimation of the pdf using euclide measure
  } ;





void PrintGeneralizedOutput(GeneralizedOutput *t) {
  int i;
  

  if (t) { 
    PRINTF("GO B %u max %u length %u val %u \n",t-> sizeinbytes,t-> max, t->length, t->val);
    PRINTF("Xorder:");
    
    switch (t->method) { 
    case HoltWintersMethod:
    case MovingAverageMethod:
    case MartingaleMethod:
    case CompressionMethod:
    case KernelMethodMMD:  
      PRINTF("Y     :");
      PRINTTIMESERIES(t->y,0);
      PRINTF("\nPValue:"); 
      PRINTTIMESERIES(t->pvalue,0);
      break;
    case NonParametricMethod:
      PRINTF("D");
      PRINTRESULTDISTANCE(t->y->data[0].y,t->pvalue->data[0].y);
      break;
    default:
      break;
      
    }
    switch (t->val) { 
    case 4:
      PRINTF("\nDM    :");
      PRINTTIMESERIES(t->dm,0);
            
    case 3: 
      PRINTF("\nYM    :");
      PRINTTIMESERIES(t->ym,0);
      PRINTF("\n");
      break;
    }
  }
}
int  PrintGeneralizedOutputVertical(GeneralizedOutput *t) {
  int i;
  int alert =0;

  if (t) { 
    PRINTF("GO B %u max %u length %u val %u \n",t-> sizeinbytes,t-> max, t->length, t->val);
    
    
    if (t->method==HoltWintersMethod || t->method==MovingAverageMethod ) {
      PRINTF("Xorder:\tY\tPValue\tYM\tDM\n");
      
      for (i=0;i<t->length;i++) {
	PRINTSP(t->y->data[i])
	PRINTSP(t->pvalue->data[i])
	if (t->val==4) {
	  PRINTSP(t->ym->data[i])
	  PRINTSP(t->dm->data[i])
	}
	if ((t->pvalue->data[i].y[0]) 
	    >
	    0.98) { 
	  PRINTF("\t##### ALERT "); 
	  alert =1;
	}
	PRINTF("\n");
	
	
      }
    }
    
    else { 
      PRINTF("X %ld\n",t->y->data[0].x);
      alert = PRINTRESULTDISTANCEVERTICAL(t->y->data[0].y,t->pvalue->data[0].y,0.5, 0.95);
    }
    PRINTF("\n");
  }
  return alert;
}

int scalarF(TimeSeries *stream,
	    int Methods,
	    void **state,
	    GeneralizedOutput **out,
	    Mat *parameters) { 

  GeneralizedOutput *ot = 0;
  TimeSeries *y,*d,*r;
  int i;
  Mat temp;
  
  *out = ot;
  
  switch (Methods) { 
  case HoltWintersMethod:

    
    if (debug) { 
      PRINTF("Holt-Winters methods\n");
    }
    
    if (HoltWinters(stream,
		    (HoltWinterState **)state,
		    &y,
		    &d,
		    parameters[PPLACE], 
		    parameters[ALPHAPLACE], 
		    parameters[BETAPLACE], 
		    parameters[GAMMAPLACE])) {
    
      SinglePoint *temp_sp;
      double temp;
      double distance; 
      double max_distance;



      (*out) = ot = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));
      ot -> max = y->max;
      ot -> length = stream ->length;
      ot -> method = HoltWintersMethod;
      ot->ym = y ;
      ot->dm = d ;

      ALLOCATE_SP(temp_sp,stream->dimensions);

      ALLOCATETS(ot->y,y->max,1);   
      ALLOCATETS(ot->pvalue,y->max,1);   
      /*
      COPY_TS(ot->y,y);
      COPY_TS(ot->pvalue,y);
      */

      for (i=0;i<stream ->length;i++) { 
	distance    = euclidean_distance(stream->data[i], ot ->ym->data[i]);
	max_distance= euclidean_distance(ot ->ym->data[i],
					 SP_mul(temp_sp,parameters[KPLACE],
						SP_add(temp_sp,
						       ot ->ym->data[i],
						       ot ->dm->data[i])));  
	ot->pvalue->data[i].x = ot->y->data[i].x = stream->data[i].x;
	switch ((int) parameters[SIDESPLACE]) {
	case PLUSSIDE:
	case MINUSSIDE:
	case BOTHSIDE:
	default: 
	  ot->y->data[i].y[0] = (distance>=max_distance)?1:0;
	  break;
	}
	temp = distance/max_distance;
	
	ot->pvalue->data[i].y[0] = (temp<1)?(0):(1 - 1/temp);

      }
      
      /*
	FREETS(y);
	FREETS(d);
      */

      FREE_SP(temp_sp);
      if (debug) PrintGeneralizedOutput(ot);
      return 1;
    }
    else { 

      return 0;
    }
    break;
  case KernelMethodMMD:  

     if (debug) { 

      PRINTF("Kernel Methods\n ");
#ifdef RDEF
  R_FlushConsole(); 
#endif

     }
     if (kernelMethods(stream,
		       (NonParametricState **)state,
		       &y,&d,
		       (int)parameters[NPLACE],(int)parameters[MPLACE],
		       (int)parameters[KERNELMETHODPLACE],(int)parameters[KFUNCPLACE]))
       {
	 

	 (*out) = ot = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));
	 ot->length=y->length;
	 ot -> method = KernelMethodMMD;
	 
	 ot -> y    =   y;
	 ot->pvalue =   d;

	 if (debug) { 
	   PRINTF(" time step %ld \n", stream->data[0].x);
	   PrintGeneralizedOutput(ot);
	   
	 }
	 
	 return 1;
       }
     else {
       ot = (*out) = 0;
       return 0;
     }
     break;


   case NonParametricMethod:
     if (debug) { 

      PRINTF("Non parametric Method\n ");
#ifdef RDEF
  R_FlushConsole(); 
#endif

     }
     if (nonParametricDistance(stream,
			       (NonParametricState **)state,
			       &y,&d,
			       (int)parameters[NPLACE],(int)parameters[MPLACE],(int)parameters[DFPLACE],
			       (int)parameters[TOPPLACE]))
       {
	 

	 (*out) = ot = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));
	 ot->length=y->length;
	 ot -> method = NonParametricMethod;
	 
	 ot -> y    =   y;
	 ot->pvalue =   d;

	 if (debug) { 
	   PRINTF(" time step %ld \n", stream->data[0].x);
	   PrintGeneralizedOutput(ot);
	   
	 }
	 
	 return 1;
       }
     else {
       ot = (*out) = 0;
       return 0;
     }
     break;
   case CompressionMethod:
     if (debug) { 

      PRINTF("Compression Method\n ");
#ifdef RDEF
  R_FlushConsole(); 
#endif

     }
     if (compressionDistance(stream,
			     (KolmogorovState **)state,
			     &y,&d,
			     (int)parameters[NPLACE],(int)parameters[MPLACE],(int)parameters[DFPLACE], (int) parameters[QUORUMPLACE]))
       {
	 
	 

	 (*out) = ot = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));

	 ot->length=y->length;
	 ot -> method = CompressionMethod;
	 
	 ot -> y    =  y;
	 ot->pvalue =  d;

	 // PrintGeneralizedOutput(ot);

	 return 1;
       }
     else {
       ot = (*out) = 0;
       return 0;
     }
     break;
   case MartingaleMethod:
     if (debug) { 

      PRINTF("Martingale Method\n ");

     }

     if (
	 conformal_prediction_martingale
	 (stream,
	  (MartingaleState **)state,
	  &y,&d,&r,
	  // epsilon,    t,             lambda,        fraction
 	  parameters[0], parameters[1], parameters[2], parameters[3],
	  // M
	  (int)parameters[4],
	  global_array[((int)parameters[5]) % N_PVALUES_METHODS] // actual strangeness method method 
	  )
	 ) 
       {
	 

	 (*out) = ot = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));
	 ot->length=y->length;
	 ot -> method = MartingaleMethod;
	 
	 if (debug) { 
	   PRINTF("Martingale time series \n");
	   PRINTTIMESERIES(y,0);
	   PRINTF("PVAL time series \n");
	   PRINTTIMESERIES(d,0);
	   PRINTF("State\n");
	   PRINTMARTINGALESTATE(*(MartingaleState **)state);
	 }
	 ot -> y      =  r; // response
	 ot->pvalue   =  d; // pvalue 
	 ot->ym       =  y; // Martingale   
	 // PrintGeneralizedOutput(ot);
	 
	 return 1;
       }
     else {
       ot = (*out) = 0;
       return 0;
     }
     break;
  case MovingAverageMethod:
    if (debug) { 

      PRINTF("Moving Average  Method\n ");
#ifdef RDEF
  R_FlushConsole(); 
#endif

     }

    if (MovingAverage(stream,
		      (MovingAverageState **)state,
		      &y,
		      &d,
		      (int)parameters[PPLACE], 
		      parameters[KPLACE])) {

      SinglePoint *temp_sp;
      double temp;
      double distance; 
      double max_distance;

      (*out) = ot = (GeneralizedOutput *) calloc(1,sizeof(GeneralizedOutput));
      ot->max=stream ->length;
      ot->val = 4;
      ot -> length = stream ->length;
      ot -> method = MovingAverageMethod;
      ot -> ym     =  y;
      ot -> dm     = d;
  
      ALLOCATE_SP(temp_sp,stream->dimensions);

      ALLOCATETS(ot->y,y->max,1);   
      ALLOCATETS(ot->pvalue,y->max,1);   
      /*
      COPY_TS(ot->y,y);
      COPY_TS(ot->pvalue,y);
      */
      for (i=0;i<stream ->length;i++) { 
	distance    = euclidean_distance(stream->data[i], ot ->ym->data[i]);
	max_distance= euclidean_distance(ot ->ym->data[i],
					 SP_mul(temp_sp,parameters[KPLACE],
						SP_add(temp_sp,
						       ot ->ym->data[i],
						       ot ->dm->data[i])));  
	ot->pvalue->data[i].x = ot->y->data[i].x = stream->data[i].x;
	switch ((int) parameters[SIDESPLACE]) {
	case PLUSSIDE:
	case MINUSSIDE:
	case BOTHSIDE:
	default: 
	  ot->y->data[i].y[0] = (distance>=max_distance)?1:0;
	  break;
	}
	temp = distance/max_distance;
	
	ot->pvalue->data[i].y[0] = (temp<1)?(0):(1 - 1/temp);

      }

      
      FREE_SP(temp_sp);
      return 1;
    }
    else { 
      return 0;
    }
    break;
    








  default:
    return 0;
    break;
  }

  return 0;
}

/* this is the basic routine for an basic interface 
 * 
 *
 */



int scalarFBASIC(timestamp  *x, double *y, int *n, int *dimensions,
		 int *method, 
		 double *params, int *np,
		 timestamp  *xout, // 1 dimensional
		 double *yout,     // 1 dimensional
		 double *pvalue,   // 1 dimensional
		 double *ym,       // it can be dimensions
		 double *dm,       // as above
		 int *outn,
		 char **stateString) { 
  
  void *state=0;
  int result;
  int i;
  int scan=0;
  int localmethod=0;
  GeneralizedOutput *oo=0;
  char *s;
  TimeSeries *ts;
  SinglePoint tempSP;

  if (debug) { 

    PRINTF(" inside scalarFBASIC \n");
    PRINTF(" n %d method %d dimensions %d \n",*n,*method,*dimensions);
    for (i=0;i<*n;i++) {
      PRINTF(" x %ld y %e  \n",x[i],y[i]);
    }

  }

 
  if ((*method)<0) {
    scan =1 ;
    localmethod = -1*(*method);
    method = &localmethod;
    PRINTF(" inside scalarFBASIC \n");
    PRINTF(" n %d method %d dimensions %d \n",*n,*method,*dimensions);
  }

  *outn=0;

  ALLOCATETS(ts,*n,*dimensions);
  tempSP.code = 0;
  tempSP.dimensions = *dimensions;

  for (i=0;i<ts->max;i++) { 
    int j;
    tempSP.x = (Xorder) x[i];
    tempSP.y = (Mat *) y + i*(*dimensions); 

    COPYSP(ts->data[i], tempSP);

  }
  if (debug) { 
    PRINTTIMESERIES(ts,0);
    
  }
  

  ts->length = ts->max = *n;
  if (*stateString && strlen(*stateString)>10) {
    if (debug) {
      PRINTF(" Read State - %s - %lud \n",*stateString, strlen(*stateString));/* Register routines, allocate resources. */
    }

    state = READ_STATE(*method,stateString);
  }
  //R_ProcessEvents();
  if (debug) { 
    PRINTF(" ScalarF Call method = %d\n", *method);/* Register routines, allocate resources. */
  }

  if ( (*method == NonParametricMethod || *method ==CompressionMethod ||  *method ==KernelMethodMMD)  
       && ts->length> params[NPLACE]
       ) { 
    int intervals = ts->length/params[MPLACE];
    int space= MAX((int)params[MPLACE],(int)params[NPLACE] ) ;
    int j;
    int location,delta;
    
    int counter = 0;
    TimeSeries *temp;
    
    if (debug) { 
      PRINTF(" ScalarF Call method = %d intervals %d\n", *method,intervals);/* Register routines, allocate resources. */
    }

    
    ALLOCATETS(temp,space,*dimensions);
    
    // INITIALIZE STATE: REFERENCE WINDOW
    copy_ts_b(temp,ts, 0,  (int)params[NPLACE]);
    result = scalarF(temp,*method,&state,&oo,(Mat *)params);

    delta = (int)params[NPLACE] - (int)params[MPLACE];
    space = (int)params[MPLACE];
    
    j = 1;
    location = j*space+delta;
    

    for (j=1,location = j*space+delta; location <ts->length ;j++, location = j*space+delta ) {
      
      int border = (ts->length-(location)<space)?ts->length-(location):space; 
    
  
    
      copy_ts_b(temp,ts, location,  location+border);

      if (debug) {
	PRINTF(" times %d j*space %d \n", j,location);
	PRINTTIMESERIES(temp,0);

      }
      

      result = scalarF(temp,*method,&state,&oo,(Mat *)params);


      if (result) {
	
	if (*method == NonParametricMethod) { 
	  double *tem_result = filtering_quorum_separate(oo->y->data[0].y,oo->pvalue->data[0].y,params[QUORUMPLACE]) ;
	  if (debug) {  
	    PRINTF("Quorum d %e pv %e  \n",tem_result[0],tem_result[1]);/* Register routines, allocate resources. */
	  }

	  yout[counter]   = (double) tem_result[0];
	  xout[counter]   = (timestamp) oo->y->data[0].x;
	  pvalue[counter] = (double) tem_result[1];
	  

	  free(tem_result);
	} 
	if (*method ==CompressionMethod || *method ==KernelMethodMMD) { 
	  yout[counter]   = (double) oo->y->data[0].y[0];
	  xout[counter]   = (timestamp) oo->y->data[0].x;
	  pvalue[counter] = (double) oo->pvalue->data[0].y[0];
	  
	}
	FREE_GEN_OUTPUT(oo);
	if (debug) {  
	  PRINTF("x %ld y %e pv %e \n",xout[counter],yout[counter],pvalue[counter]);/* Register routines, allocate resources. */
	}
      }
      counter ++;

      if (scan) { 
	if (space > 10  )
	  space = 10;

	delta = 0;
	scan  = 0;
      }



    }
    
    FREETS(temp);
    


  }
  else { 
    if (debug) { PRINTF(" classic methods .... \n"); }
    result = scalarF(ts,*method,&state,&oo,(Mat *)params);
  }


  if (debug) { 
    PRINTF(" ScaqlarF Call Done result %d \n",result );/* Register routines, allocate resources. */
  }
  if (result && oo) {

    *outn = oo->length;
    if (debug) {PRINTF(" result != 0  %d \n",oo->length ); }
    for (i=0;i<oo->length;i++ ) { 

      yout[i]   = (double) oo->y->data[i].y[0];
      xout[i]   = (timestamp) oo->y->data[i].x;
      pvalue[i] = (double) oo->pvalue->data[i].y[0];
      if (debug) { 
	PRINTF("x %ld y %e pv %e \n",xout[i],yout[i],pvalue[i]);/* Register routines, allocate resources. */
      }
      if (*method == HoltWintersMethod || *method == MovingAverageMethod) { 
	int _N = oo->ym->data[i].dimensions;
	int j;
	for (j=0;j<_N;j++) { 
	  ym[i*_N+j] = (double) oo->ym->data[i].y[j];
	  dm[i*_N+j] = (double) oo->dm->data[i].y[j];
	  if (debug) { 
	    PRINTF("ym  %e  dm %e ",ym[i*_N+j],dm[i*_N+j]);/* Register routines, allocate resources. */
	  }
	}
	if (debug) PRINTF("\n");/* Register routines, allocate resources. */

      }
      if (*method == MartingaleMethod) { 
	ym[i] = (double) oo->ym->data[i].y[0];
	if (debug) { 
	  PRINTF("ym  %e  \n",ym[i]);/* Register routines, allocate resources. */
	}
      }
      
    }
    if (debug) { 
      PRINTF(" Update the output \n");/* Register routines, allocate resources. */
    }
     
    FREE_GEN_OUTPUT(oo);
  }
  
  if (debug) { 
    PRINTF(" FREED output  \n");/* Register routines, allocate resources. */
  }
  FREETS(ts);
  if (debug) { 
    PRINTF(" WRITE STATE \n");/* Register routines, allocate resources. */
  }
  

  s = WRITE_STATE(*method,state);
  if (debug) { 
    PRINTF(" WRITE STATE %s\n",s);/* Register routines, allocate resources. */
  }

  
  if (debug) { 
    PRINTF(" FREE STATE %s\n",s);/* Register routines, allocate resources. */
  }
  if (state) 
    FREE_STATE(*method,state);
  
  
#ifdef RDEF

  if (debug) { 
    PRINTF(" clean up previous state string len %d -%s- PREV -%s- \n",strlen(s),s,stateString[0]);/* Register routines, allocate resources. */ 
    R_FlushConsole(); 
  }
  if (s) free(s);
  //Free(stateString[0]);
  //stateString[0] = (char*) Calloc(strlen(s),char); 
  //PRINTF(" COPY STATE STRING \n");/* Register routines, allocate resources. */   
  //R_FlushConsole(); 
  //strcpy(stateString[0],s); 

#endif
#ifndef RDEF
  
  if (debug) { 
    
    PRINTF(" WRITE STATE \n%s\n",s);/* Register routines, allocate resources. */
    PRINTF(" change of gloves %s-\n",*stateString);/* Register routines, allocate resources. */                                                                                                                                                            
  }
  if (*stateString) { 
    free(*stateString);

  }
  *stateString = s;
  
#endif

  if (debug) { 
    
    PRINTF(" Free state string \n");/* Register routines, allocate resources. */
    
  }

  
  return result;
}


