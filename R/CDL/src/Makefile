##################################################################
#
# Makefile 
#
#
#

# Code root directory 



HOMEDIR = /home/paolo/Desktop/FastCDL/FastLibrary_9_Multidimensional
#HOMEDIR = /home/paolo/Martingale/FastLibrary_7/
RDIR = $(HOMEDIR)/R/CDL/src/
RCOMMAND=/usr/bin/R

## C and FORTAN compilers. I use gcc but you may change it. 
CC  = gcc
FF  = gfortran
AR  = ar rcs

## Machine Specific optimizations 
MACHTYPE = x86_64
ARCH = -D$(MACHTYPE)
#PROCESSOR=athlon

ifeq ($(PROCESSOR),athlon)
	OPT_PROC = -O2 -Wall   -msse2  -march=opteron -mtune=opteron -fPIC
	#OPT_PROC = -O2  -fPIC# -msse2  -march=opteron -mtune=opteron
else 
	OPT_PROC = -O1    -fPIC
	#OPT_PROC = -O2   -fPIC# -msse2  -march=opteron -mtune=opteron #-O2   -msse2 
endif


# Santanu mode (smaller set) 
# SETOFNONPARAMETRICMETHODS=-DSANTANU_HISTOGRAM
# All method no pvalue 
# SETOFNONPARAMETRICMETHODS=-DPVALUE_MODE_0
# Medium sise set
SETOFNONPARAMETRICMETHODS=-DREGULAR_MODE
# 


OPT = $(OPT_PROC) -DRDEF $(SETOFNONPARAMETRICMETHODS) 




# Includes
INC = -I $(HOMEDIR)/include  \
	-I $(HOMEDIR)/NonParametric/DistanceFunctions -I $(HOMEDIR)/NonParametric \
	-I $(HOMEDIR)/NonParametric/PValue \
	-I $(HOMEDIR)/Sort -I $(HOMEDIR)/Summation -I $(HOMEDIR)/Window  \
	-I $(HOMEDIR)/TimeSeries/HoltWinters   -I $(HOMEDIR)/TimeSeries \
	-I $(HOMEDIR)/TimeSeries/MovingAverage \
	-I $(HOMEDIR)/TimeSeries/Martingale \
	-I $(HOMEDIR)/TimeSeries/Compression \
	-I $(HOMEDIR)/R/R-2.10.1/include/ \
	-I $(HOMEDIR)/R/R-2.10.1/src/include/ \
	-I $(HOMEDIR)/Poset \
	-I $(HOMEDIR)/TimeSeries/Kernels \
	-I $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5 \
	-I $(PWD)  


## Libraries 
math = -lm 
cdl  = -L$(HOMEDIR)/lib/ -l cdl \
	-L$(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5 -l bz2  -lm # compression library 


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
objTimeseries =  $(HOMEDIR)/TimeSeries/timeseries.o $(HOMEDIR)/TimeSeries/HoltWinters/holtwinters.o $(HOMEDIR)/TimeSeries/MovingAverage/movingaverage.o \
	$(HOMEDIR)/TimeSeries/Martingale/martingale.o $(HOMEDIR)/TimeSeries/Martingale/nnstrangeness.o $(HOMEDIR)/TimeSeries/Martingale/pdf.o $(HOMEDIR)/TimeSeries/Compression/compress.o $(HOMEDIR)/TimeSeries/Kernels/kernels.o
objNonParame  =  $(HOMEDIR)/NonParametric/nonparametric.o $(HOMEDIR)/NonParametric/DistanceFunctions/distance.o $(HOMEDIR)/NonParametric/DistanceFunctions/interface.o $(HOMEDIR)/NonParametric/PValue/pvalue.o 
obj =  $(HOMEDIR)/Summation/doubly_compensated_sumc.o $(HOMEDIR)/Sort/quicksort.o   $(HOMEDIR)/Poset/minimumspanningtree.o $(HOMEDIR)/Window/window.o $(HOMEDIR)/Poset/poset.o $(objTimeseries) $(objNonParame) 

#objTimeseries =  TimeSeries/timeseries.o TimeSeries/HoltWinters/holtwinters.o TimeSeries/MovingAverage/movingaverage.o \
#	TimeSeries/Martingale/martingale.o TimeSeries/Martingale/nnstrangeness.o TimeSeries/Compression/compress.o
#objNonParame  =  NonParametric/nonparametric.o NonParametric/DistanceFunctions/distance.o NonParametric/DistanceFunctions/interface.o NonParametric/PValue/pvalue.o
#obj =  Summation/doubly_compensated_sumc.o Sort/quicksort.o   Window/window.o $(objTimeseries) $(objNonParame)



lib: $(obj)
	$(AR) $(HOMEDIR)/lib/libcdl.a $(obj) $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5/libbz2.a
	cp $(HOMEDIR)/lib/libcdl.a .

all:
	make lib
	$(RCOMMAND) CMD SHLIB cdlR.c $(INC) $(cdl)  -o cdlR.so
	#	$(HOMEDIR)/R/R-2.10.1/bin/R CMD SHLIB cdlR.c $(INC) $(cdl)  -o cdlR.so
R:cdlR.o
	make lib
	$(RCOMMAND) CMD SHLIB cdlR.c $(INC) $(cdl)  -o cdlR.so 
	#$(HOMEDIR)/R/R-2.10.1/bin/R CMD SHLIB cdlR.c $(INC) $(cdl)  -o cdlR.so 
	cp cdlR.so $(HOMEDIR)/R/R-2.10.1/lib


## Example 2 is used to check whether or not an MM algorithm implementation is correct 
 
alonehist: Examples/standalonehistogram.o  $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/standalonehistogram.o $(obj) -o Executable/histfile  $(math) $(cdl)

hist: Examples/histogram.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/histogram.o $(obj) -o Executable/hist  $(math) $(cdl)

time: Examples/timeserie1.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie1.o $(obj) -o Executable/time1  $(math) $(cdl)

error: Examples/error.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/error.o $(obj) -o Executable/error  $(math) $(cdl)

time2: Examples/timeserie2.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie2.o $(obj) -o Executable/time2  $(math) $(cdl)
time3: Examples/timeserie3.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie3.o $(obj) -o Executable/time3  $(math) $(cdl)

timeSama: Examples/timeseriesSama.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeseriesSama.o $(obj) -o Executable/timeseriesSama  $(math) $(cdl)



clean: 
	rm cdlR.o
#	rm -f $(obj) $(HOMEDIR)/Examples/*.o $(HOMEDIR)/Executable/* $(HOMEDIR)/*.o.c $(HOMEDIR)/*/*.o.c $(HOMEDIR)/*/*/*.o.c $(HOMEDIR)/lib/*
