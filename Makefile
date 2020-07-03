##################################################################
#
# Makefile 
#
#
#

# Code root directory 

HOMEDIR= $(PWD)


## C and FORTAN compilers. I use gcc but you may change it. 
CC  = gcc
FF  = gfortran
AR  = ar rcs

OPT_PROC =  -Wall -Winline -O2 -g   -fPIC  

## Machine Specific optimizations 
#MACHTYPE = x86_64
#ARCH = -D$(MACHTYPE)
#PROCESSOR=athlon

#ifeq ($(PROCESSOR),athlon)
#	#OPT_PROC = -O2 -Wall   -msse2  -march=opteron -mtune=opteron -fPIC
#	OPT_PROC = -g -fPIC
#else 
#	#OPT_PROC = -O2   -msse2 -fPIC
#	OPT_PROC = -g   -fPIC # -O2   -msse2 
#endif


# Santanu mode (smaller set) 
#SETOFNONPARAMETRICMETHODS=-DSANTANU_HISTOGRAM
# All method no pvalue 
# SETOFNONPARAMETRICMETHODS=-DPVALUE_MODE_0
# Medium sise set
SETOFNONPARAMETRICMETHODS=-DREGULAR_MODE
# 


#OPT = $(OPT_PROC) $(ARCH) $(SETOFNONPARAMETRICMETHODS)
OPT = $(OPT_PROC)  $(SETOFNONPARAMETRICMETHODS) 




# Includes
INC = -I $(HOMEDIR)/include  \
	-I $(HOMEDIR)/NonParametric/DistanceFunctions -I $(HOMEDIR)/NonParametric \
	-I $(HOMEDIR)/NonParametric/PValue \
	-I $(HOMEDIR)/Sort -I $(HOMEDIR)/Summation -I $(HOMEDIR)/Window  \
	-I $(HOMEDIR)/TimeSeries/HoltWinters   -I $(HOMEDIR)/TimeSeries \
	-I $(HOMEDIR)/TimeSeries/MovingAverage \
	-I $(HOMEDIR)/TimeSeries/Martingale \
	-I $(HOMEDIR)/R/R-2.10.1/src/include/ \
	-I $(HOMEDIR)/R/R-2.10.1/include/ \
	-I $(HOMEDIR)/TimeSeries/Compression \
	-I $(HOMEDIR)/Poset \
	-I $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5 \
	-I $(HOMEDIR)/TimeSeries/Kernels \



## Libraries 
math = -lm 
cdl  = -L$(HOMEDIR)/lib/ -l cdl \
	-L$(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5 -l bz2  \
	-lm


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
objTimeseries =  $(HOMEDIR)/TimeSeries/timeseries.o $(HOMEDIR)/TimeSeries/HoltWinters/holtwinters.o $(HOMEDIR)/TimeSeries/MovingAverage/movingaverage.o  $(HOMEDIR)/TimeSeries/Kernels/kernels.o \
	$(HOMEDIR)/TimeSeries/Martingale/martingale.o $(HOMEDIR)/TimeSeries/Martingale/nnstrangeness.o $(HOMEDIR)/TimeSeries/Martingale/pdf.o $(HOMEDIR)/TimeSeries/Compression/compress.o
objNonParame  =  $(HOMEDIR)/NonParametric/nonparametric.o $(HOMEDIR)/NonParametric/DistanceFunctions/distance.o $(HOMEDIR)/NonParametric/DistanceFunctions/interface.o $(HOMEDIR)/NonParametric/PValue/pvalue.o 
obj =  $(HOMEDIR)/Summation/doubly_compensated_sumc.o $(HOMEDIR)/Sort/quicksort.o   $(HOMEDIR)/Window/window.o $(HOMEDIR)/Poset/poset.o $(HOMEDIR)/Poset/minimumspanningtree.o $(objTimeseries) $(objNonParame) 

#objTimeseries =  TimeSeries/timeseries.o TimeSeries/HoltWinters/holtwinters.o TimeSeries/MovingAverage/movingaverage.o \
#	TimeSeries/Martingale/martingale.o TimeSeries/Martingale/nnstrangeness.o TimeSeries/Compression/compress.o
#objNonParame  =  NonParametric/nonparametric.o NonParametric/DistanceFunctions/distance.o NonParametric/DistanceFunctions/interface.o NonParametric/PValue/pvalue.o
#obj =  Summation/doubly_compensated_sumc.o Sort/quicksort.o   Window/window.o $(objTimeseries) $(objNonParame)

code:
	make clean
	tar czvf fastcdl.tar.gz myudfs/ Perl/ Java/ python/ R/CDL Poset/ include/ Window/ Sort/ Summation/ NonParametric/ TimeSeries/ Makefile  lib/ README.standaloneHistogramComparisonForRegression
	#echo tar zcvf ./fastcdl.tar.gz  ./README.standaloneHistogramComparisonForRegression lib/ ./Makefile TimeSeries/  NonParametric/ Summation/ Sort/ Window/  include/ Poset/ R/CDL/ python/ Examples/ Java/ Perl/  myudfs/ 

zlib: $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5/libbz2.a
	#cd $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5/
	#make clean libbz2.a 
	#cd $(HOMEDIR)
	bash $(HOMEDIR)/bzip.sh $(HOMEDIR)
lib: $(obj)
	mkdir -p $(HOMEDIR)/lib
	make zlib 
	$(AR) $(HOMEDIR)/lib/libcdl.a $(obj) $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5/libbz2.a 	
#	$(CC) -shared -o $(HOMEDIR)/lib/libcdl.so $(obj) $(HOMEDIR)/TimeSeries/Compression/bzip2-1.0.5/libbz2.a

## Example 2 is used to check whether or not an MM algorithm implementation is correct 
 
alonehist: Examples/standalonehistogram.o  $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/standalonehistogram.o $(obj) -o Executable/histfile   $(cdl) 
compareall: Examples/compareall.o  $(obj)
	make  lib
	$(CC) $(OPT) $(INC) Examples/compareall.o $(obj) -o Executable/compareall  $(cdl)

example2: Examples/example.2.o  $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/example.2.o  -o Executable/hist_perf   $(cdl)

hist: Examples/histogram.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/histogram.o $(obj) -o Executable/hist   $(cdl)

time: Examples/timeserie1.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie1.o $(obj) -o Executable/time1   $(cdl)

error: Examples/error.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/error.o $(obj) -o Executable/error   $(cdl)

time2: Examples/timeserie2.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie2.o $(obj) -o Executable/time2   $(cdl)
time4: Examples/timeserie4.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie4.o $(obj) -o Executable/time4   $(cdl)
time3: Examples/timeserie3.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeserie3.o $(obj) -o Executable/time3   $(cdl)

timeSama: Examples/timeseriesSama.o $(obj)
	make lib
	$(CC) $(OPT) $(INC) Examples/timeseriesSama.o $(obj) -o Executable/timeseriesSama   $(cdl)





clean: 
	rm -f $(obj) Examples/*.o Executable/* ./*.o.c ./*/*.o.c ./*/*/*.o.c lib/*
