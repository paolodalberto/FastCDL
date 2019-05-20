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
#include <Python.h>


#define REGULAR_MODE 1

#include "pvalue.h"
#include "type.h"
#include "distance.h"
#include "interface.h"
#include "sort.h"
#include "quicksort.h"
#include "doubly_compensated_sumc.h"
#include "timeseries.h"
#include "holtwinters.h"
#include "nonparametric.h"
#include "window.h"
#include "string.h"

//#ifndef REGULAR_MODE
//#define N_METHODS 10
//#endif


//#define PRINTSTATE 1

static int debug = 0;

PyObject *anomaly_holtwinters (PyObject *self, PyObject *args);
PyObject *anomaly_series (PyObject *self, PyObject *args);
PyObject *anomaly_series_basic (PyObject *self, PyObject *args);
PyObject *spam_sum (PyObject *self, PyObject *args);
PyObject *anomaly_histogram_comp (PyObject *self, PyObject *args);


//#define N_METHODS 18

/* Methods are mumbered in this order (from 1 to 18):
   phiofA 
   KsiofA 
   KolmogorovSmirnov 
   KullbackLeiberI 
   KullbackLeiberJ 
   JinK 
   JinL 
   JensenShannonDivergence 
   ChiSquare 
   Hellinger 
   Bhattacharyya 
   CramerVonMises 
   Euclide 
   Camberra 
   GeneralizedKs 
   GeneralizedKr 
   GeneralizedK2s 
   Minkowsky 
 */

static PyObject *parse_output(GeneralizedOutput *output, unsigned int type);
static void parse_parameters (const char *keys[], Mat *values, PyObject *params);
static void parse_parameters_a (Mat *values, PyObject *params);
static void parse_timeseries_tuple (PyObject *tuple, long *x, double *y, int dim);
static void parse_and_copy_timeseries(PyObject *series, TimeSeries *timeseries,int size);

#ifdef PRINTSTATE
static void debug_save_state (HoltWinterState *state, const char *filename);
#endif /* PRINTSTATE */

/**
 * An array of functions made available through the module.
 */
static PyMethodDef anomalymethods[] = {
    { "holtwinters", anomaly_holtwinters, METH_VARARGS, "Run Holt-Winters anomaly detection on a series." },
    { "general_anomaly", anomaly_series,  METH_VARARGS, "Run scalarF anomaly detection on a series." },
    { "histogram_comp", anomaly_histogram_comp, METH_VARARGS, "Run function to calculate similarity between histograms."},
    { NULL, NULL, 0, NULL }
};

/**
 * Initialization function for the module.
 */
void initanomaly (void)
{
    Py_InitModule ("anomaly", anomalymethods);
}


PyObject *anomaly_histogram_comp (PyObject *self, PyObject *args)
{
    PyObject *hist1;
    PyObject *hist2;
    PyObject *tuple;
    PyObject *tuple2;
    PyObject *result;
    
    double *res;
    //FILE *file;
    char *tempName;

    //    res = (double *) malloc (sizeof(double) * N_METHODS*2);
    //res = (double *) calloc (2,sizeof(double) );
    tempName = (char *) malloc (sizeof(char) * 1024);

    if (debug) printf(" anomaly_histogram_comp \n");


    //file = fopen("histogramDistance.t", "a");   

    if (! PyArg_ParseTuple (args, "OO", &hist1, &hist2))
        return NULL;

    if (hist1 == NULL || hist2 == NULL)
    {
        PyErr_SetString (PyExc_ValueError, "null histogram");
        return NULL;
    }

    if (debug) printf(" %d - %d \n",(int)PyList_Size (hist1) , (int)PyList_Size (hist2));

    if ( (PyList_Size (hist1) > 0) && (PyList_Size (hist2)>0) )
    {
        Histogram *h1 = NULL;
        Histogram *h2 = NULL;
        int i;
        unsigned int size1 = PyList_Size (hist1);
        unsigned int size2 = PyList_Size (hist2);
        Mat summation =0;


        if (hist1 != Py_None && hist2 != Py_None)
        {
            h1 = (Histogram *) malloc (sizeof(Histogram));
            h2 = (Histogram *) malloc (sizeof(Histogram));
            h1->word = (char **) malloc (sizeof(char *) * size1);
            h2->word = (char **) malloc (sizeof(char *) * size2);
            h1->number = (Mat *) malloc (sizeof(Mat) * size1);
            h2->number = (Mat *) malloc (sizeof(Mat) * size2);

            h1->length = size1;
            h2->length = size2;
            if (debug) printf("%d %d\n", size1, size2);
            //            sleep(1);

            for(i = 0; i < size1; i++)
            {
	      //if (debug) printf("ITERATION  [%d]\n", i);
	      tuple = PyList_GetItem (hist1, i);
	      tempName = PyString_AsString ( PyTuple_GetItem (tuple, 0) );
	      //if (debug) printf("ITERATION  [%d] x %s\n", i,tempName);
	      h1->word[i] = malloc( (strlen(tempName)+1) * sizeof(char) );
	      strcpy(h1->word[i], tempName);
	      h1->number[i] = (Mat ) PyFloat_AsDouble ( PyTuple_GetItem (tuple, 1) );
	      
	      summation += h1->number[i];
	      if (debug) printf("1- %d: %lf - [%s]\n", i, h1->number[i], h1->word[i]);
            }
            // empty histogram ... no histogram
            if (summation == 0.0) { 
                if (debug)
                    printf("histogram 1 is zero \n");

                PyErr_SetString (PyExc_ValueError, "empty histogram");
                return NULL; 
            }

            summation =0;

            for(i = 0; i < size2; i++)
            {
                tuple2 = PyList_GetItem (hist2, i);
                tempName = PyString_AsString ( PyTuple_GetItem (tuple2, 0) );
                h2->word[i] = malloc( (strlen(tempName)+1) * sizeof(char) );
                strcpy(h2->word[i], tempName);
                h2->number[i] = (Mat) PyFloat_AsDouble ( PyTuple_GetItem (tuple2, 1) );
                summation += h2->number[i];
                if (debug) printf("2- %d: %lf - [%s]\n", i, h2->number[i], h2->word[i]);
            }
            if (summation == 0.0) { 
                if (debug)
                    printf("histogram 2 is zero \n");

                PyErr_SetString (PyExc_ValueError, "empty histogram");
                return NULL; 
            }

            if (debug) {
                for(i = 0; i < ((size1>size2)?size2:size1); i++)
                    printf("%d: %lf - [%s]\t %lf - [%s]\n", i, h1->number[i], h1->word[i], h2->number[i], h2->word[i]);

                printf("Computing histogram \n");
            }
	    // res = distanceHistograms(h1, h2);
            res = distanceHistogramsQ(h1, h2);
	    
	    if (debug) 
	      printf(" res: %lf, %lf\n", res[0], res[1]);

	    freeHistogramContents(h1);  
	    freeHistogramContents(h2);  

	    result = Py_BuildValue ("dd", res[0], res[1]);
	    free(res);

            return result;


        }

    }
    //    fclose(file);

    return Py_BuildValue ("");
}


/**
 * Generates a new time series based upon the Holt-Winters algorithm.
 * This function accepts a previous state as the first argument, a single tuple or list of tuples
 * as the second argument, and a dictionary of configuration parameters (optional). The second
 * argument is the input time series and must consist of x-y value tuples where x is a long and y
 * is a double.
 * @param self NULL pointer.
 * @param args tuple containing the arguments passed to this function.
 * @return tuple containing the new state and the output time series.
 */
PyObject *anomaly_holtwinters (PyObject *self, PyObject *args)
{
    /* Define valid param keys. */
    static const char *parameter_keys[] = { "periodicity", "alpha", "beta", "gamma", "k", "type", NULL };

    PyObject *dimensions; // change this to string
    PyObject *state; // change this to string
    PyObject *series;
    PyObject *params;
    PyObject *anomaly;
    PyObject *result;
    //FILE *file;
    //char *tempState = NULL;

    //file = fopen("AnomalyModuleLog.t", "a");

    /* Extract three arguments passed to the python function. */
    if (! PyArg_ParseTuple (args, "OOO!O",  &state, &series, &PyDict_Type,&params,&dimensions))
        return NULL;

    /* Check the types of the first two argument not checked by PyArg_ParseTuple. */
    if (state == NULL || (state != Py_None && ! PyString_Check (state)))
    {
        PyErr_SetString (PyExc_TypeError, "expected string at argument 1");
        return NULL;
    }
    if (series == NULL || (! PyTuple_Check (series) && ! PyList_Check (series)))
    {
        PyErr_SetString (PyExc_TypeError, "expected tuple or list at argument 2");
        return NULL;
    }

    if (PyTuple_Check (series) || PyList_Size (series) > 0)
    {
        Mat p[] = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE };
        HoltWinterState *state_in = NULL;
        HoltWinterState *state_out = NULL;
        TimeSeries *timeseries = NULL;
        GeneralizedOutput *output = NULL;
        unsigned int size = PyTuple_Check (series) ? 1 : PyList_Size (series);
        char *tempState;
	int dim=1;
 
	if (dimensions != Py_None)
        {
	  dim = (int)PyInt_AsLong(dimensions);
        }
	/* Extract any user defined algorithm override values. */
        if (params != NULL)
        {
	  
            parse_parameters (parameter_keys, p, params);
        }

        /* Create a new state if provided by the user. */
        if (state != Py_None)
        {
            unsigned int s = PyString_Size (state);
            state_in = (HoltWinterState *) malloc (s);
            //tempState = (char *) malloc (s);
            tempState = PyString_AsString (state);
            //memcpy (state_in, PyString_AsString (state), s); CHECK THIS LATER !!!!!!
            //fprintf(file, "tempState: [%s]\n", tempState);
            state_in = ReadHWState(0, &tempState);
            
        }

        /* Extract the time series data into the TimeSeries data structure. */
        ALLOCATETS (timeseries, size,dim);
	parse_and_copy_timeseries(series,timeseries,size);

	if (debug) { 
	  printf ("periodicity: %f alpha: %f beta: %f gamma: %f k: %f type: %f\n", p[0], p[1], p[2], p[3], p[4], p[5]);
	  PRINTTIMESERIES(timeseries,0);
	}

	
        /* Generate the anomaly detection time series. */
        scalarF (timeseries, HoltWintersMethod, (void **) &state_in, &output, p); // state_in is modified

        /* Save the new state to return to the caller. */
        tempState = PrintHWState(state_in,0,0);

	if (debug) { 
	  printf(" State %s", tempState);

	}

#ifdef PRINTSTATE
        debug_save_state (state_out, "a.bin");
#endif /* PRINTSTATE */

        //state = PyString_FromStringAndSize ((const char *) state_out, state_out->sizeinbytes);
        //tempState = PrintHWState(state_out, 0, 0);
        //fwrite(tempState, sizeof(char), strlen(tempState), file);
        //fprintf(file, "\n\n");
        state = PyString_FromString(tempState);
        //PRINTHWSTATE(state_out, 0);

	anomaly = parse_output(output,0);
	
        /* Free all memory used to generate the anomaly time series. */
        free (state_out);
        FREE_HW_STATE (state_in);
        FREETS (timeseries);
        FREE_GEN_OUTPUT (output);
        free(tempState);

        /* Create a tuple consisting of the new state and the anomaly time series. */
        result = PyTuple_New (2);
        PyTuple_SetItem (result, 0, state);
        PyTuple_SetItem (result, 1, anomaly);

        //fclose(file);
        return result;
    }

    return Py_BuildValue ("");
}


/**
 * Generates a new time series based upon the scalarF interface algorithm.
 * This function accepts a previous state as the first argument, a single tuple or list of tuples
 * as the second argument, and a dictionary of configuration parameters (optional). The second
 * argument is the input time series and must consist of x-y value tuples where x is a long and y
 * is a double.
 * @param self NULL pointer.
 * @param args tuple containing the arguments passed to this function.
 * @return tuple containing the new state and the output time series.
 *
 * HoltWintersMethod       0    P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }
 * NonParametricMethod     1    P = (Rsize, Wsize, 1, Ratio=0.2, MST=0/POS=1,1.0) 
 * MovingAverageMethod     2    P = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE }
 * MartingaleMethod        3    P = (e=0.93, Dt=3,MAX=20, Ratio=0.2, Wsize, 0=n^2/1=average/2=PDF) 
 * CompressionMethod       4    P = (Rsize,Wsize,1,#BOOTSTRAP)
 * KernelMethodMMD         5    P = (Rsize,Wsize,RBF=1,MMD^2=1/Linear=0)
 *
 * 
 */
PyObject *anomaly_series (PyObject *self, PyObject *args)
{
    /* Define valid param keys. */

  int dim=1; //PyObject *dimensions;
  int  type_method;   // PyObject *method;
    PyObject *state; // change this to string
    PyObject *series;
    PyObject *params;
    PyObject *anomaly;
    PyObject *result;
    //FILE *file;
    //char *tempState = NULL;
    //file = fopen("AnomalyModuleLog.t", "a");

    if (debug) printf("Anomaly Series \n");

    /* Extract three arguments passed to the python function. */
    if (! PyArg_ParseTuple (args, "iOOOi", &type_method, &state, &series,  &params,&dim))
        return NULL;

    /* Check the types of the first two argument not checked by PyArg_ParseTuple. */
    
    
    if (state == NULL || (state != Py_None && ! PyString_Check (state)))
    {
        PyErr_SetString (PyExc_TypeError, "expected string at argument 1");
        return NULL;
    }
    if (series == NULL || (! PyTuple_Check (series) && ! PyList_Check (series)))
    {
        PyErr_SetString (PyExc_TypeError, "expected tuple or list at argument 2");
        return NULL;
    }

    if (PyTuple_Check (series) || (PyList_Check(series) && PyList_Size (series) > 0))
    {
        Mat p[] = { 1, 1.0, 0.0, 0.0, 1, BOTHSIDE };

        // state in and out as a function of the method

        void *state_in = NULL;
        void *state_out = NULL;
        TimeSeries *timeseries = NULL;
        GeneralizedOutput *output = NULL;
        unsigned int size = PyTuple_Check (series) ? 1 : PyList_Size (series);
        char *tempState ;
	

	
	if (debug) printf("Dimensions %d \n",dim);

	if (debug) printf("Method %d \n",type_method);
        
        /* Extract any user defined algorithm override values. */
        if (params != NULL)
        {
            parse_parameters_a (p, params);
        }


        /* Create a new state if provided by the user. */
        if (state != Py_None)
        {
            //unsigned int s = PyString_Size (state);
	  tempState   = PyString_AsString (state);
	  if (debug) printf("State %s len %ld\n",tempState,strlen(tempState));
	  if (strlen(tempState)>0) { 
	    state_in = (void*) READ_STATE(type_method,&tempState);
	  }
	  if (debug) printf("State %s\n",tempState);

        }
	
	
	
        /* Extract the time series data into the TimeSeries data structure. */
        ALLOCATETS (timeseries, size,dim);
	parse_and_copy_timeseries(series,timeseries,size);

	
	if (debug) { PRINTTIMESERIES(timeseries,0);} 


        /* Generate the anomaly detection time series. */

        if (scalarF (timeseries, type_method, &state_in, &output, p)) // state_in is modified
	  {
	    if (debug) { printf("python scalarF done \n");} 
	    anomaly = parse_output(output,type_method);
	  }
	else 
	  anomaly = Py_None;
	
        tempState = WRITE_STATE(type_method, state_in);
        state = PyString_FromString(tempState);
	


        /* Free all memory used to generate the anomaly time series. */
        free (state_out);
        FREE_STATE (type_method,state_in);
        FREETS (timeseries);
        FREE_GEN_OUTPUT (output);
        free(tempState);

        /* Create a tuple consisting of the new state and the anomaly time series. */
	if (anomaly != Py_None) { 
	  result = PyTuple_New (2);
	  PyTuple_SetItem (result, 0, state);
	  PyTuple_SetItem (result, 1, anomaly);
	}
	else {
	  result = PyTuple_New (1);
	  PyTuple_SetItem (result, 0, state);
	}
	  //fclose(file);
        return result;
    }

    return Py_BuildValue ("");
}








/**
 * Iterates through a list of user provided parameters and overrides default values.
 * @param keys a NULL-terminated array of available parameter names.
 * @param params a dictionary of user defined parameter override values.
 * @param values an array of default values corresponding to the keys.
 */
    void
parse_parameters (const char *keys[], Mat *values, PyObject *params)
{
    unsigned int i = 0;

    /* Iterate through the keys and set values as they are found. */
    while (keys[i] != NULL)
    {
        PyObject *key = PyString_FromString (keys[i]);
        PyObject *value = NULL;

        if ((value = PyDict_GetItem (params, key)) != NULL)
        {
            if (PyInt_Check (value))
                values[i] = (double) PyInt_AsLong (value);
            else if (PyLong_Check (value))
                values[i] = PyLong_AsDouble (value);
            else if (PyFloat_Check (value))
                values[i] = PyFloat_AsDouble (value);
        }
	if (key != NULL) {
	  Py_DECREF (key);
	}
        i++;
    }
}


void parse_parameters_a(Mat *values, PyObject *params)
{
    unsigned int i = 0;
    unsigned int size = PyList_Size (params);
    if (debug) printf("size %d \n",size);
    /* Iterate through the keys and set values as they are found. */
    while (i<size)
    {
      values[i] = PyFloat_AsDouble (PyList_GetItem (params, i));
      if (debug) printf("Value[i] %e \n",values[i]);

      i++;
    }
}



/**
 * Extract the x and y values from the tuple.
 * @param tuple the x-y value pair.
 * @param x a pointer to the calling variable where the value is written.
 * @param y a pointer to the calling variable where the value is written.
 */
void
parse_timeseries_tuple (PyObject *tuple, long *x, double *y, int dim)
{
  int k;
  *x = PyLong_AsLong (PyTuple_GetItem (tuple, 0));
  if (debug) printf("x %ld\n",*x);
  for (k=0;k<dim;k++) {
    *y = PyFloat_AsDouble (PyTuple_GetItem (tuple, 1+k));
    if (debug) printf("y %e\n",*y);
    y++;
  }
}
/**
 * Extract the x and y values from the tuple.
 * @param tuple the x-y value pair.
 * @param x a pointer to the calling variable where the value is written.
 * @param y a pointer to the calling variable where the value is written.
 */
void
parse_timeseries_list (PyObject *tuple, long *x, double *y, int dim)
{
  int k;
  *x = PyLong_AsLong (PyList_GetItem (tuple, 0));
  if (debug) printf("l x %ld\n",*x);
  for (k=0;k<dim;k++) {
    *y = PyFloat_AsDouble (PyList_GetItem (tuple, 1+k));
    if (debug) printf("l y %e\n",*y);
    y++;
  }
}



void parse_and_copy_timeseries(PyObject *series, TimeSeries *timeseries, int size) {

  int i;
  
  if (debug) printf("timeseries  %d\n",size); 

  if (PyTuple_Check (series))
    {
      if (debug) printf("timeseries  Tuple %d\n",size); 
      parse_timeseries_tuple (series, 
			      &(timeseries->data[0].x), 
			      timeseries->data[0].y,
			      (int)timeseries->dimensions);
      timeseries->length += 1;
    }
  else
    {
      if (debug) printf("timeseries NOT  Tuple %d\n",size); 
      for (i = 0; i < size; i++)
	{
	  PyObject *tuple = PyList_GetItem (series, i);
	  
	  if (tuple != NULL && PyList_Check (tuple))
	    {
	      parse_timeseries_list (tuple, 
				      &(timeseries->data[i].x), 
				      timeseries->data[i].y,
				      (int)timeseries->dimensions);
	      timeseries->length += 1;
	    }
	  else if (tuple != NULL && PyTuple_Check (tuple)) {
	    if (debug) printf("timeseries element NOT  Tuple \n"); 
	    parse_timeseries_tuple (tuple, 
				    &(timeseries->data[i].x), 
				    timeseries->data[i].y,
				    (int)timeseries->dimensions);
	    timeseries->length += 1;
	    
	  }
	}
    }
}

PyObject *
parse_output(GeneralizedOutput *output, unsigned int type) { 
  int i;
  PyObject * anomaly = PyList_New (output->length);
  
  for (i = 0; i < output->length; i++)
    {
      
      PyObject *tuple = PyTuple_New ((type==2||type==0)?5:3);
      PyObject *value;
      
      value = PyLong_FromLong (output->y->data[i].x);
      PyTuple_SetItem (tuple, 0, value);
      value = PyFloat_FromDouble (output->y->data[i].y[0]);
      PyTuple_SetItem (tuple, 1, value);
      value = PyFloat_FromDouble (output->pvalue->data[i].y[0]);
      PyTuple_SetItem (tuple, 2, value);
      if (type ==0 || type ==2 ) {

	value = PyFloat_FromDouble (output->ym->data[i].y[0]);
	PyTuple_SetItem (tuple, 3, value);
	value = PyFloat_FromDouble (output->dm->data[i].y[0]);
	PyTuple_SetItem (tuple, 4, value);
      }

      PyList_SetItem (anomaly, i, tuple);
      
  
    }
  
  return anomaly;

}


#ifdef PRINTSTATE
    void
debug_save_state (HoltWinterState *state, const char *filename)
{
    FILE *file = fopen (filename, "wb");

    fwrite (state, state->sizeinbytes, 1, file);

    fclose (file);
}
#endif /* PRINTSTATE */
