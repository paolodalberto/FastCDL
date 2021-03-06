#
# ${R_HOME}/src/modules/lapack/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/modules/lapack
R_HOME = $(top_builddir)

include $(top_builddir)/Makeconf

SOURCES_C = Lapack.c # vecLibg95c.c
SOURCES_F = # vecLibg95f.f

DEPENDS = $(SOURCES_C:.c=.d)
SOURCES = $(SOURCES_C) $(SOURCES_F)
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_F:.f=.o)
HEADERS = Lapack.h

LIBSOURCES = dlamch.f dlapack0.f dlapack1.f dlapack2.f dlapack3.f dlapack4.f \
   cmplx.f
LIBOBJECTS = $(LIBSOURCES:.f=.o)

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = \
	README Makefile.in Makefile.win \
	Rlapackrc.rc dllversion.rc \
	$(HEADERS) Lapack.c \
	vecLibg95c.c vecLibg95f.f \
	cmplx.f \
	dlamch.f dlapack0.f dlapack1.f dlapack2.f dlapack3.f dlapack4.f

lapack_la = lapack$(SHLIB_EXT)
Rlapack_la = libRlapack$(DYLIB_EXT)
lapack_la_OBJECTS = $(OBJECTS)
## Used for passing '+s' to SHLIB_LINK on HP-UX.
lapack_la_LDFLAGS = 
lapack_la_LIBADD = $(LIBR) # $(LIBINTL)

Rlapack_la_OBJECTS = $(LIBOBJECTS)
Rlapack_la_LIBADD = # $(FLIBS) $(LIBR)

ALL_CFLAGS = $(ALL_CFLAGS_LO)
ALL_FFLAGS = $(ALL_FFLAGS_LO)


all: Makefile Makedeps R

Makefile: $(srcdir)/Makefile.in \
  $(top_builddir)/config.status \
  $(SOURCES)
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile $(DEPENDS)
	@cat $(DEPENDS) >> Makefile
	@touch $@

## treated separately: e.g. we need to force stores with 
## gcc >= 3.4.0 at least on i686
DLAMC_FFLAGS=$(FPICFLAGS) $(SHLIB_FFLAGS) -g -O2 -ffloat-store
dlamch.o: dlamch.f
	$(F77) $(DLAMC_FFLAGS) -c $< -o $@

R: Makefile
	@$(MAKE) Makedeps
	@$(MAKE) rhome="$(abs_top_builddir)" $(Rlapack_la) Rlapack_install
	@$(MAKE) $(lapack_la)

$(lapack_la): $(lapack_la_OBJECTS)
	$(SHLIB_LINK) -o $@ $(lapack_la_LDFLAGS) $(lapack_la_OBJECTS) $(lapack_la_LIBADD) -L$(R_HOME)/lib$(R_ARCH) -lRlapack -L$(R_HOME)/lib$(R_ARCH) -lRblas $(FLIBS)
## <FIXME>
## Not sure if we want to do this ...
	@$(MAKE) rhome="$(abs_top_builddir)" install
## </FIXME>

## Include BLAS here, as with (static) ATLAS that pulls all the
## BLAS routines into one place.
$(Rlapack_la): $(LIBOBJECTS)
	$(DYLIB_LINK) -o $@ $(LIBOBJECTS)  -L$(R_HOME)/lib$(R_ARCH) -lRblas $(Rlapack_la_LIBADD)

Rlapack_install: $(Rlapack_la) 
	@$(MKINSTALLDIRS) $(Rexeclibdir)
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(Rlapack_la) \
	  $(Rexeclibdir)/$(Rlapack_la)

install: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(lapack_la) "$(DESTDIR)$(Rexecmodulesdir)/$(lapack_la)"
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(Rlapack_la) "$(DESTDIR)$(Rexeclibdir)/$(Rlapack_la)"
installdirs:
	@$(MKINSTALLDIRS) "$(DESTDIR)$(Rexecmodulesdir)"
install-strip-Rlapack:
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(Rlapack_la) "$(DESTDIR)$(Rexeclibdir)/$(Rlapack_la)"
	@if test -n "$(STRIP_LIBS)"; then \
	  $(STRIP_LIBS) "$(DESTDIR)$(Rexeclibdir)/$(Rlapack_la)"; \
	fi
install-strip:  install-strip-Rlapack
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(lapack_la) "$(DESTDIR)$(Rexecmodulesdir)/$(lapack_la)"
	@if test -n "$(STRIP_LIBS)"; then \
	  $(STRIP_LIBS) "$(DESTDIR)$(Rexecmodulesdir)/$(lapack_la)"; \
	fi
uninstall:
	@rm -f "$(Rexecmodulesdir)/$(lapack_la)"

mostlyclean: clean
clean:
	@-rm -rf .libs _libs
	@-rm -f Makedeps *.d *.o *.a *.lo *.la \
	  lapack$(SHLIB_EXT) libRlapack$(DYLIB_EXT)
distclean: clean
	@-rm -f Makefile
maintainer-clean: distclean

TAGS info dvi check:

distdir: $(DISTFILES)
	@for f in $(DISTFILES); do \
	  test -f $(distdir)/$${f} \
	    || ln $(srcdir)/$${f} $(distdir)/$${f} 2>/dev/null \
	    || cp -p $(srcdir)/$${f} $(distdir)/$${f}; \
	done

## Automagically generated dependencies:

Lapack.o: Lapack.c ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  Lapack.h ../../../src/include/R_ext/RS.h ../../../src/include/Rconfig.h \
  ../../../src/include/R_ext/Lapack.h ../../../src/include/R_ext/BLAS.h \
  ../../../src/include/Rmodules/Rlapack.h \
  ../../../src/include/R_ext/Rdynload.h
