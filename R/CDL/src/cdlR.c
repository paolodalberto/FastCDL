#include <R.h>
//#include <cdl.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
//#define GETTIME

#define CDLRINTERFACE_MODULE
#include <cdlR.h>



static int debug =0;
 
void
R_init_mylib(DllInfo *info)
{
  Rprintf(" WWWW \n");/* esRegister routines, allocate resources. */
  R_FlushConsole();
  // R_ProcessEvents();
}
void
R_unload_mylib(DllInfo *info)
{
  /* Release resources. */
}

void  CompareCDFs(double *y1, int *len1,
		  double *y2, int *len2,
		  double *threshold, 
		  double *mindistance, 
		  double *pvalue) { 
  
  double *result;
  int N = (*len1>*len2)?*len1:*len2;
  
  double *res = DFMeasure(y1,y2, N,res);
  result = filtering_quorum_t(res,result,*threshold);
  
  if (res) { free(res); res=0;}

  *mindistance= result[0];
  *pvalue = result[1];
  
  free(result);
    
}



void  CompareHistogram(char **x1, double *y1, int *len1,
		       char **x2, double *y2, int *len2,
		       double *threshold, 
		       double *mindistance, 
		       double *pvalue) { 
  
  
  double *res = distanceHistogramsQPerl(x1,y1, *len1,
					x2,y2, *len2,
					*threshold);
  
  
  *mindistance= res[0];
  *pvalue = res[1];
  
  free(res);
    
}

void  CompareHistogramNumeric(
			      double *x1, double *y1, int *len1,
			      double *x2, double *y2, int *len2,
			      double *threshold, 
			      double *mindistance, 
			      double *pvalue) { 
  

  double *res = distanceHistogramsQPerlNumericLabel(x1,y1, *len1,
						    x2,y2, *len2,
						    *threshold);
  
  
  *mindistance= res[0];
  *pvalue = res[1];
  
  free(res);
    
}

void ExtendedCompareHistogram(char **x1, double *y1, int *len1,
			      char **x2, double *y2, int *len2,
			      double *threshold,
			      double *distance,double *pvalues,
			      int *outlen,
			      double *mindistance, 
			      double *pvalue
			      ) { 
  

  double *res = extendedCompareHistogram(x1, y1, *len1,
					 x2, y2, *len2,
					 *threshold,
					 distance,
					 pvalues,
					 outlen);
  
  *mindistance= res[0];
  *pvalue = res[1];
  
  free(res);

}




void scalarFRBASIC(timestamp *x, double *y, int *n, int *dimensions,
		  int *method, 
		  double *params, int *np,
		  timestamp *xout, 
		  double *yout, 
		  double *pvalue, 
		  double *ym, 
		  double *dm, 
		  int *outn,
		  char **stateString) { 

  int res = scalarFBASIC(x,y,n,dimensions,method,params,np,xout,yout,pvalue,ym,dm,outn,stateString);
  

} 


