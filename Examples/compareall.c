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
#define GETTIME 1

//#define SANTANU_HISTOGRAM 1

#include <cdl.h>
#include <type.h>
#include <assert.h>
#include <distance.h>
#include <interface.h>
#include <sort.h>
#include <quicksort.h>
#include <stdlib.h>
#include <stdio.h>
#include <doubly_compensated_sumc.h>
#include <pvalue.h>
#include <string.h>
/* 
 PREV: leds 1 in 1 turned 1 on 1 prospect 1 park 1 holiday 1
 CURR: leds 1 in 1 turned 1 on 1 prospect 1 park 1 holiday 1 


char *w1[7] = {"leds", "in", "turned", "on", "prospect", "park", "holiday" };
char *w2[7] = {"leds",  "turned",  "prospect", "in","park", "holiday","on" };
int n1[7] = {1,2,3,4,1,3,4};
int n2[7] = {1,1,1,1,1,1,1};
 */


#define ID_SEP 1
#define FEATURE_SEP 4
#define VALUE_SEP   19


#define MAXSTRINGSIZE  1024*1024*2
#define MAXHISTOGRAMS  8*1024  

int readHistogram(char *line, 
		  Histogram *hist, 
		  char **ids, 
		  int len) { 
  int histcount = 0;
  int featurecount =0;
  char *tail;
  char *snumber;
  int csize=len;

  
  
  while (*line && histcount<csize) {
    
    char *start;
    int j;

    while ( *line != '(' ) { 
      line ++;
    }
    line++;

    featurecount =1;

    
    ids[histcount] = line;
    
    while (*line != ID_SEP) { 
      line++;
      //printf("%s\n",line);
    }
    *line = 0;
    //printf("######%s\n", ids[histcount]);

    
    tail = ++line;
    
    while (*tail !=')')  { 
      tail++ ;
      if (*tail == FEATURE_SEP) { 
	featurecount ++;
      }
    }
    //printf("feature count %d\n", featurecount);

    if (featurecount==0) continue;

    if (hist[histcount].length < featurecount) { 
      hist[histcount].length = featurecount;
      if ( hist[histcount].word) free( hist[histcount].word);
      if ( hist[histcount].number) free( hist[histcount].number);

      hist[histcount].word = (char **) malloc(featurecount*sizeof(char **));
      hist[histcount].number = (Mat *) malloc(featurecount*sizeof(Mat));      
      
      
    }
    tail = line;


    for (j=0;j<featurecount;j++) {
    
      hist[histcount].word[j] = tail;
      
      while (*tail !=VALUE_SEP)  { 
	//printf("%d\n",*tail);
	tail ++;
	
      }


      *tail = 0;
      //printf("######%s\n", hist[histcount].word[j]);
      snumber = ++tail;
      
      while (*tail != FEATURE_SEP && *tail != ')') { 
	//printf("%d\n",*tail);
	tail ++;
      }
      *tail = 0;
      tail++;

      hist[histcount].number[j] = (double)atoi(snumber);
      //printf("######%e\n", hist[histcount].number[j]);
	    
    }
    line = tail;
    
    histcount ++;
    //printf("###### H %d C %d \n", histcount,csize);
    
  }

  //printf("###### DONE \n");

  return histcount;
} 


int main(int argc, char** argv) { 
  int i,l;
  double time;
  int samples;
  char line[MAXSTRINGSIZE];
  Histogram *docs = ( Histogram *) calloc(MAXHISTOGRAMS,sizeof(Histogram)); 
  char *docsId[MAXHISTOGRAMS];
  int count ;
  double *res;
  int linecount =0;
  int comparisons=0;
  int j =0;
  
  
  double threshold;

  if (argc>1) 
    threshold = atof(argv[1]);
  else 
    threshold = 0.2;
  
  START_CLOCK;
  while (
	 scanf("%s",line)!=EOF  && 
	 scanf("%d",&count)!=EOF 
	 ) { 
    int c = readHistogram(line,
			  docs,
			  docsId,
			  count);
    linecount ++;
    if (linecount==1000) { 
      
      END_CLOCK;
      fprintf(stderr,"\t Comparisons %d\n",comparisons);
      comparisons=0;
      linecount =0;
      START_CLOCK;

    }
    for (i=0;i<c; i++) { 
      for (j=i+1; j<c; j++) { 
	comparisons++;
	res = distanceHistogramsQQ(docs+i,docs +j, threshold);
	if (res[1]< 0.95) 
	  printf("%s\t%s\t\%f\t%f\n",docsId[i],docsId[j], res[0],res[1]);
	free(res);
      }
      
    }
    
    
  }
  END_CLOCK;
  
  for (i=0;i<4096;i++) { 
    if ( docs[i].word  ) free(docs[i].word  );
    if ( docs[i].number) free(docs[i].number);

  }
  free(docs);
  
  return 0;
}
