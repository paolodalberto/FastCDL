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



#ifdef PRINTSTATE
static void debug_save_state (HoltWinterState *state, const char *filename);
#endif /* PRINTSTATE */

/**
 * An array of functions made available through the module.
 */
static PyMethodDef anomalymethods[] = {
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
    PyObject *met;
    PyObject *tuple;
    PyObject *tuple2;
    PyObject *result;

    double *res;
    //FILE *file;
    double method;
    char *tempName;

    //    res = (double *) malloc (sizeof(double) * N_METHODS*2);
    //res = (double *) calloc (2,sizeof(double) );
    tempName = (char *) malloc (sizeof(char) * 1024);

    if (debug) printf(" anomaly_histogram_comp \n");


    //file = fopen("histogramDistance.t", "a");   

    if (! PyArg_ParseTuple (args, "OOO", &hist1, &hist2, &met))
        return NULL;

    if (met != NULL)
        method = (double) PyFloat_AsDouble (met);
    else
        method = 0.5;

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
                tuple = PyList_GetItem (hist1, i);
                tempName = PyString_AsString ( PyTuple_GetItem (tuple, 0) );
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
            res = distanceHistogramsQQ(h1, h2,method);
	    
            /*if(method > N_METHODS)
            {
                PyErr_SetString (PyExc_ValueError, "invalid method number");
                return NULL;
		}
            if (debug) 
	      printf("meth: %d, pos: %d, res: %lf, %lf\n", method, method-1, res[method-1], res[method+N_METHODS-1]);

            return( Py_BuildValue ("dd", res[method-1], res[method+N_METHODS-1]) );
	    */
	    if (debug) 
	      printf(" res: %lf, %lf\n", res[0], res[1]);


	    result = Py_BuildValue ("dd", res[0], res[1]);
	    free(res);
	    freeHistogramContents(h1);  
	    freeHistogramContents(h2);  

            return result;


        }

    }
    //    fclose(file);

    return Py_BuildValue ("");
}



