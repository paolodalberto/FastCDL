#include <R.h>
#include <cdl.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


#define CDLRINTERFACE_MODULE
#include <cdlR.h>


void
R_init_mylib(DllInfo *info)
{
  Rprintf(" WWWW \n");/* Register routines, allocate resources. */
  R_FlushConsole();
  // R_ProcessEvents();
}
void
R_unload_mylib(DllInfo *info)
{
  /* Release resources. */
}

void scalarFR(int *x, double *y, int *n, 
	      int *method, 
	      double *params, int *np,
	      int *xout, double *ym, double *pvalue, 
	      char **stateString) { 
  
  void *state;
  int result;
  int i;
  GeneralizedOutput *oo;
  
  TimeSeries *ts;

  ALLOCATETS(ts,*n);
  for (i=0;i<ts->max;i++) {
    ts->x[i] = (Xorder) x[i];
    ts->y[i] = (Mat )   y[i];
  }
  ts->length = ts->max = *n;

  state = READ_STATE(*method,stateString);

  Rprintf(" State %s \n",*stateString);/* Register routines, allocate resources. */
  R_FlushConsole();
  //R_ProcessEvents();

  result = scalarF(ts,*method,&state,&oo,params);

  
  for (i=0;i<oo->length;i++ ) { 
    ym[i]   = (double) oo->y[i];
    xout[i] = (int) oo->x[i];
    pvalue[i] = (double) oo->pvalue[i];
  }

  FREE_GEN_OUTPUT(oo);
  FREETS(ts);
  
  *stateString = WRITE_STATE(*method,state);
  
}
