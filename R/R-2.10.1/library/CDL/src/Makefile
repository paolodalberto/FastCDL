##################################################################
#
# Makefile 
#
#
#

# Code root directory 



HOMEDIR = $(PWD)/../../../
RDIR = $(PWD)

## C and FORTAN compilers. I use gcc but you may change it. 
CC  = gcc
FF  = gfortran
AR  = ar rcs

## Machine Specific optimizations 

ARCH = -D$(MACHTYPE)

ifeq ($(PROCESSOR),athlon)
	OPT_PROC = -O2 -Wall   -msse2  -march=opteron -mtune=opteron -fPIC
else 
	OPT_PROC = -O2   -msse2 -fPIC
endif


# Santanu mode (smaller set) 
# SETOFNONPARAMETRICMETHODS=-DSANTANU_HISTOGRAM
# All method no pvalue 
# SETOFNONPARAMETRICMETHODS=-DPVALUE_MODE_0
# Medium sise set
SETOFNONPARAMETRICMETHODS=-DREGULAR_MODE
# 


OPT = $(OPT_PROC) $(ARCH) $(SETOFNONPARAMETRICMETHODS) 




# Includes
INC = -I $(HOMEDIR)/include  \
	-I $(HOMEDIR)/NonParametric/DistanceFunctions -I $(HOMEDIR)/NonParametric \
	-I $(HOMEDIR)/NonParametric/PValue \
	-I $(HOMEDIR)/Sort -I $(HOMEDIR)/Summation -I $(HOMEDIR)/Window  \
	-I $(HOMEDIR)/TimeSeries/HoltWinters   -I $(HOMEDIR)/TimeSeries \
	-I $(HOMEDIR)/TimeSeries/MovingAverage \
	-I $(HOMEDIR)/TimeSeries/Martingale \
	-I $(HOMEDIR)/R/R-2.10.1/include/ \
	-I $(PWD)


## Libraries 
math = -lm 
cdl  = -L$(HOMEDIR)/lib/ -l cdl # this is the final library


#################################################################################
#################################################################################
#  
#  Below this point you should not need to modify anything :)
#
#
#
#
#################################################################################
#################################################################################


## Default rule to build the object files

.c.o:
#	$(CC) -E $(OPT) $(INC) $< -o $@.c
	$(CC) -c $(OPT) $(INC) $< -o $@


## defaults object files
objTimeseries =  $(HOMEDIR)/TimeSeries/timeseries.o $(HOMEDIR)/TimeSeries/HoltWinters/holtwinters.o $(HOMEDIR)/TimeSeries/MovingAverage/movingaverage.o $(HOMEDIR)/TimeSeries/Martingale/martingale.o
objNonParame  =  $(HOMEDIR)/NonParametric/nonparametric.o $(HOMEDIR)/NonParametric/DistanceFunctions/distance.o $(HOMEDIR)/NonParametric/DistanceFunctions/interface.o $(HOMEDIR)/NonParametric/PValue/pvalue.o
obj =  $(HOMEDIR)/Summation/doubly_compensated_sumc.o $(HOMEDIR)/Sort/quicksort.o   $(HOMEDIR)/Window/window.o $(objTimeseries) $(objNonParame) 



lib: $(obj)
	$(AR) $(HOMEDIR)/lib/libcdl.a $(obj)

R:cdlR.o
	make lib
	$(HOMEDIR)/R/R-2.10.1/bin/R CMD SHLIB cdlR.c -L $(HOMEDIR)/lib -l cdl
	cp cdlR.so $(HOMEDIR)/R/R-2.10.1/lib
## Example 2 is used to check whether or not an MM algorithm implementation is correct 
 
alonehist: $(HOMEDIR)/Examples/standalonehistogram.o  $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/standalonehistogram.o $(obj) -o $(HOMEDIR)/Executable/histfile  $(math) $(cdl)

hist: $(HOMEDIR)/Examples/histogram.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/histogram.o $(obj) -o $(HOMEDIR)/Executable/hist  $(math) $(cdl)

time: $(HOMEDIR)/Examples/timeserie1.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/timeserie1.o $(obj) -o $(HOMEDIR)/Executable/time1  $(math) $(cdl)

error: $(HOMEDIR)/Examples/error.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/error.o $(obj) -o $(HOMEDIR)/Executable/error  $(math) $(cdl)

time2: $(HOMEDIR)/Examples/timeserie2.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/timeserie2.o $(obj) -o $(HOMEDIR)/Executable/time2  $(math) $(cdl)
time3: $(HOMEDIR)/Examples/timeserie3.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/timeserie3.o $(obj) -o $(HOMEDIR)/Executable/time3  $(math) $(cdl)

timeSama: $(HOMEDIR)/Examples/timeseriesSama.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) $(HOMEDIR)/Examples/timeseriesSama.o $(obj) -o $(HOMEDIR)/Executable/timeseriesSama  $(math) $(cdl)




clean: 
	rm -f $(obj) $(HOMEDIR)/Examples/*.o $(HOMEDIR)/Executable/* $(HOMEDIR)/*.o.c $(HOMEDIR)/*/*.o.c $(HOMEDIR)/*/*/*.o.c $(HOMEDIR)/lib/*
