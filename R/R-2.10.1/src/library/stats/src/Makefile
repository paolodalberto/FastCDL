#
# ${R_HOME}/src/library/stats/src/Makefile

pkg = stats


srcdir = .
top_srcdir = ../../../..

top_builddir = ../../../..
subdir = src/library/$(pkg)/src
R_HOME = $(top_builddir)
R_SHARE_DIR = $(R_HOME)/share
R_INCLUDE_DIR = $(R_HOME)/include

SOURCES_C = init.c kmeans.c \
  ansari.c bandwidths.c chisqsim.c d2x2xk.c fexact.c kendall.c ks.c \
  line.c smooth.c \
  prho.c swilk.c \
  ksmooth.c loessc.c isoreg.c Srunmed.c Trunmed.c \
  dblcen.c distance.c hclust-utils.c \
  nls.c \
  HoltWinters.c PPsum.c arima.c burg.c filter.c \
  mAR.c pacf.c starma.c port.c family.c sbart.c \
  approx.c loglin.c lowess.c massdist.c splines.c

SOURCES_F = bsplvd.f bvalue.f bvalus.f loessf.f ppr.f qsbart.f \
  sgram.f sinerp.f sslvrg.f stxwx.f \
  hclust.f kmns.f \
  eureka.f stl.f portsrc.f lminfl.f
DEPENDS = $(SOURCES_C:.c=.d)
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_F:.f=.o)

# need config.h
PKG_CPPFLAGS =-I../../../include -I$(top_srcdir)/src/include -DHAVE_CONFIG_H
PKG_CFLAGS=$(C_VISIBILITY)
PKG_FFLAGS=$(F77_VISIBILITY)

SHLIB = $(pkg).so

## <FIXME>
## We should really add FLIBS to SHLIB_LIBADD, but we need to include
## the shlib Makeconf at a later stage, and its default value would
## override our setting.
PKG_LIBS = -L$(R_HOME)/lib$(R_ARCH) -lRblas  -L/usr/lib/gcc/x86_64-redhat-linux/3.4.3 -lg2c -lm
## </FIXME>

all: Makefile Makedeps
	@$(MAKE) Makedeps
	@$(MAKE) shlib

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile $(DEPENDS)
	@cat $(DEPENDS) >> Makefile
	@touch $@

shlib: $(SHLIB)
	@$(MKINSTALLDIRS) \
	  $(top_builddir)/library/$(pkg)/libs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(SHLIB) \
	  $(top_builddir)/library/$(pkg)/libs/$(SHLIB)

include $(top_srcdir)/share/make/shlib.mk

mostlyclean: clean
clean:
	@-rm -rf .libs _libs
	@-rm -f Makedeps *.d *.o *$(SHLIB_EXT)
distclean: clean
	@-rm -f Makefile
maintainer-clean: distclean

## Automagically generated dependencies:
init.o: init.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h ctest.h eda.h family.h modreg.h \
  mva.h nls.h port.h stats.h ts.h ../../../../include/R_ext/Rdynload.h \
  ../../../../include/R_ext/Visibility.h
kmeans.o: kmeans.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h modreg.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/PrtUtil.h
ansari.o: ansari.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rmath.h ctest.h \
  stats.h
bandwidths.o: bandwidths.c
chisqsim.o: chisqsim.c ../../../../include/Rmath.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Random.h \
  ../../../../include/R_ext/Applic.h ../../../../include/R_ext/RS.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/BLAS.h ../../../../include/R_ext/Complex.h \
  ctest.h ../../../../include/R.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Constants.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h
d2x2xk.o: d2x2xk.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rmath.h ctest.h
fexact.o: fexact.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ctest.h ../../../../include/Rmath.h
kendall.o: kendall.c ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rmath.h ctest.h
ks.o: ks.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rmath.h ctest.h
line.o: line.c ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  eda.h
smooth.o: smooth.c ../../../../include/R_ext/Boolean.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  eda.h
prho.o: prho.c ../../../../include/Rmath.h \
  ../../../../include/R_ext/Boolean.h ctest.h ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h
swilk.o: swilk.c ../../../../include/Rmath.h \
  ../../../../include/R_ext/Boolean.h ctest.h ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h
ksmooth.o: ksmooth.c ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h
loessc.o: loessc.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h
isoreg.o: isoreg.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h modreg.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/PrtUtil.h
Srunmed.o: Srunmed.c modreg.h ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h
Trunmed.o: Trunmed.c modreg.h ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h
dblcen.o: dblcen.c mva.h ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h
distance.o: distance.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rmath.h mva.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/PrtUtil.h \
  stats.h
hclust-utils.o: hclust-utils.c ../../../../include/R_ext/Boolean.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Complex.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/PrtUtil.h \
  ../../../../include/R_ext/Print.h ../../../../include/R_ext/Utils.h \
  mva.h ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Constants.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/RS.h
nls.o: nls.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h nls.h
HoltWinters.o: HoltWinters.c ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
PPsum.o: PPsum.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
arima.o: arima.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
burg.o: burg.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
filter.o: filter.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
mAR.o: mAR.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/R_ext/Applic.h \
  ../../../../include/R_ext/BLAS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
pacf.o: pacf.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
starma.o: starma.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ts.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h stats.h
port.o: port.c port.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  ../../../../include/R_ext/PrtUtil.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h ../../../../include/R_ext/RS.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Constants.h \
  ../../../../include/R_ext/BLAS.h
family.o: family.c ../../../../include/Rinternals.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  ../../../../include/R_ext/PrtUtil.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Constants.h family.h
sbart.o: sbart.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rmath.h \
  ../../../../include/R_ext/PrtUtil.h modreg.h \
  ../../../../include/Rinternals.h
approx.o: approx.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Applic.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/RS.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/BLAS.h \
  ../../../../include/R_ext/Complex.h
loglin.o: loglin.c ../../../include/config.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Applic.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/RS.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/BLAS.h ../../../../include/R_ext/Complex.h
lowess.o: lowess.c ../../../include/config.h ../../../../include/Rmath.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Applic.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/BLAS.h \
  ../../../../include/R_ext/Complex.h ../../../../include/R_ext/Utils.h
massdist.o: massdist.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Applic.h ../../../../include/R_ext/Boolean.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/BLAS.h \
  ../../../../include/R_ext/Complex.h
splines.o: splines.c ../../../include/config.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Applic.h ../../../../include/R_ext/Boolean.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/BLAS.h \
  ../../../../include/R_ext/Complex.h
