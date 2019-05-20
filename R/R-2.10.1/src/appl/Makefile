##
## ${R_HOME}/src/appl/Makefile


srcdir = .
top_srcdir = ../..

top_builddir = ../..
subdir = src/appl

include $(top_builddir)/Makeconf

## <NOTE>
## All functions/subroutines to be called via .C() or .Fortran() must
## also have an entry in the appropriate table/array in
## @file{../main/registration.c} in order to be registered with the
## symbol lookup mechanism, i.e., 'pre-dyn.loaded'.
## </NOTE>

SOURCES_C = \
	bakslv.c binning.c \
	cpoly.c cumsum.c \
	fft.c fmin.c integrate.c interv.c \
	lbfgsb.c \
	machar.c maxcol.c \
	pretty.c \
	rcont.c rowsum.c \
	stem.c strsignif.c \
	tabulate.c \
	uncmin.c \
	zeroin.c
SOURCES_F = \
	ch2inv.f chol.f \
	dchdc.f dpbfa.f dpbsl.f dpoco.f dpodi.f dpofa.f dposl.f dqrdc.f \
	dqrdc2.f dqrls.f dqrsl.f dqrutl.f dsvdc.f dtrco.f dtrsl.f \
	eigen.f
DEPENDS = $(SOURCES_C:.c=.d)
OBJECTS_BLAS =  blas.o  cmplxblas.o
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_F:.f=.o) \
  # $(OBJECTS_BLAS)
HEADERS =

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = COPYRIGHT COPYRIGHT.ftn README \
	Makefile.in Makefile.win \
	$(HEADERS) \
	$(SOURCES_C) \
	$(SOURCES_F)

#ALL_CFLAGS = $(ALL_CFLAGS_LO)
#ALL_FFLAGS = $(ALL_FFLAGS_LO)

Rblas_la = libRblas$(DYLIB_EXT)
Rblas_la_LIBADD = # -flat_namespace -undefined suppress

all: Makefile Makedeps R

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status $(SOURCES_C)
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile $(DEPENDS)
	@cat $(DEPENDS) >> Makefile
	@touch $@

R: Makefile
	@$(MAKE) Makedeps
	@$(MAKE) libappl.a

libappl.a: $(OBJECTS)
	rm -f $@
	$(AR) cr $@ $(OBJECTS)
	$(RANLIB) $@

blas.o: $(top_srcdir)/src/extra/blas/blas.f
	$(F77) $(ALL_FFLAGS) -c $< -o $@
cmplxblas.o: $(top_srcdir)/src/extra/blas/cmplxblas.f
	$(F77) $(ALL_FFLAGS) -c $< -o $@

mostlyclean: clean
clean:
	@-rm -rf .libs _libs
	@-rm -f Makedeps *.d *.o *.a *.lo *.la stamp-lo $(Rblas_la)

distclean: clean
	@-rm -f Makefile
maintainer-clean: distclean

install install-strip uninstall TAGS info dvi check:

distdir: $(DISTFILES)
	@for f in $(DISTFILES); do \
	  test -f $(distdir)/$${f} \
	    || ln $(srcdir)/$${f} $(distdir)/$${f} 2>/dev/null \
	    || cp -p $(srcdir)/$${f} $(distdir)/$${f}; \
	done

## Automagically generated dependencies:

bakslv.o: bakslv.c ../../src/include/config.h \
  ../../src/include/Rinternals.h ../../src/include/R_ext/Arith.h \
  ../../src/include/R_ext/libextern.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/Complex.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/Memory.h ../../src/include/R_ext/PrtUtil.h \
  ../../src/include/R_ext/Print.h ../../src/include/R_ext/Utils.h \
  ../../src/include/R_ext/Linpack.h ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Applic.h
binning.o: binning.c ../../src/include/config.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/Arith.h \
  ../../src/include/R_ext/libextern.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
cpoly.o: cpoly.c ../../src/include/config.h \
  ../../src/include/R_ext/Arith.h ../../src/include/R_ext/libextern.h \
  ../../src/include/R_ext/Memory.h ../../src/include/Rmath.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
cumsum.o: cumsum.c ../../src/include/config.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
fft.o: fft.c ../../src/include/config.h ../../src/include/Rmath.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
fmin.o: fmin.c ../../src/include/Rmath.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
integrate.o: integrate.c ../../src/include/config.h \
  ../../src/include/Rinternals.h ../../src/include/R_ext/Arith.h \
  ../../src/include/R_ext/libextern.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/Complex.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/Memory.h ../../src/include/R_ext/PrtUtil.h \
  ../../src/include/R_ext/Print.h ../../src/include/R_ext/Utils.h \
  ../../src/include/Rmath.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/BLAS.h
interv.o: interv.c ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/BLAS.h ../../src/include/R_ext/Complex.h \
  ../../src/include/R_ext/Utils.h
lbfgsb.o: lbfgsb.c ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/Linpack.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/PrtUtil.h \
  ../../src/include/R_ext/Print.h
machar.o: machar.c ../../src/include/config.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
maxcol.o: maxcol.c ../../src/include/config.h \
  ../../src/include/R_ext/Arith.h ../../src/include/R_ext/libextern.h \
  ../../src/include/Rmath.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/Random.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
pretty.o: pretty.c ../../src/include/config.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h ../../src/include/Rmath.h
rcont.o: rcont.c ../../src/include/R_ext/Random.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h ../../src/include/R_ext/Utils.h
rowsum.o: rowsum.c ../../src/include/config.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
stem.o: stem.c ../../src/include/config.h ../../src/include/Rmath.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/Print.h \
  ../../src/include/R_ext/Utils.h ../../src/include/R_ext/Complex.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/BLAS.h
strsignif.o: strsignif.c ../../src/include/config.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/Memory.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/BLAS.h ../../src/include/R_ext/Complex.h \
  ../../src/include/Rmath.h
tabulate.o: tabulate.c ../../src/include/config.h \
  ../../src/include/R_ext/Arith.h ../../src/include/R_ext/libextern.h \
  ../../src/include/R_ext/Applic.h ../../src/include/R_ext/Boolean.h \
  ../../src/include/R_ext/RS.h ../../src/include/Rconfig.h \
  ../../src/include/R_ext/Error.h ../../src/include/R_ext/BLAS.h \
  ../../src/include/R_ext/Complex.h
uncmin.o: uncmin.c ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/BLAS.h ../../src/include/R_ext/Complex.h \
  ../../src/include/R_ext/PrtUtil.h ../../src/include/R_ext/Print.h \
  ../../src/include/R_ext/Linpack.h ../../src/include/Rmath.h
zeroin.o: zeroin.c ../../src/include/R_ext/Applic.h \
  ../../src/include/R_ext/Boolean.h ../../src/include/R_ext/RS.h \
  ../../src/include/Rconfig.h ../../src/include/R_ext/Error.h \
  ../../src/include/R_ext/BLAS.h ../../src/include/R_ext/Complex.h
