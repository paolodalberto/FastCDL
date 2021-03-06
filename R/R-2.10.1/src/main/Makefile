#
# ${R_HOME}/src/main/Makefile


srcdir = .
top_srcdir = ../..

top_builddir = ../..
subdir = src/main
# next is needed for shared BLAS
R_HOME=$(top_builddir)

include $(top_builddir)/Makeconf

SOURCES_C = \
	CConverters.c CommandLineArgs.c \
	Rdynload.c Renviron.c RNG.c \
	agrep.c apply.c arithmetic.c array.c attrib.c \
	base.c bind.c builtin.c \
	character.c coerce.c colors.c complex.c connections.c context.c \
	cov.c cum.c \
	dcf.c datetime.c debug.c deparse.c deriv.c devices.c \
	dotcode.c dounzip.c dstruct.c duplicate.c \
	engine.c envir.c errors.c eval.c \
	format.c fourier.c \
	gevents.c gram.c gram-ex.c gramRd.c graphics.c grep.c \
	identical.c inlined.c inspect.c internet.c iosupport.c \
	lapack.c list.c localecharset.c logic.c \
	main.c mapply.c match.c memory.c model.c \
	names.c \
	objects.c optim.c optimize.c options.c \
	par.c paste.c pcre.c platform.c \
	plot.c plot3d.c plotmath.c \
	print.c printarray.c printvector.c printutils.c qsort.c \
	random.c raw.c registration.c relop.c rlocale.c \
	saveload.c scan.c seq.c serialize.c size.c sort.c source.c split.c \
	sprintf.c startup.c subassign.c subscript.c subset.c summary.c sysutils.c \
	unique.c util.c \
	version.c vfonts.c

SOURCES_F =  xxxpr.f

## If the substitutes are needed, the corresponding objects are put by
## configure in  
EXTRA_SOURCES_C = \
	Rmain.c \
	alloca.c acosh.c asinh.c atanh.c \
	mkdtemp.c \
	snprintf.c strdup.c strncasecmp.c \
	vsnprintf.c xspline.c

DEPENDS = $(SOURCES_C:.c=.d) $(EXTRA_SOURCES_C:.c=.d)
SOURCES = $(SOURCES_C) $(SOURCES_F)
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_F:.f=.o)  
HEADERS = \
	RBufferUtils.h Rstrptime.h \
	arithmetic.h \
	basedecl.h \
	par-common.c \
	qsort-body.c \
	rlocale_data.h \
	unzip.h

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = Makefile.in Makefile.win \
	$(HEADERS) \
	$(SOURCES_C) \
	$(EXTRA_SOURCES_C) \
	$(SOURCES_F) \
	gram.y \
	gramRd.y

ZLIB_CPPFLAGS =  -I$(top_srcdir)/src/extra/zlib
BZLIB_CPPFLAGS =  -I$(top_srcdir)/src/extra/bzip2
PCRE_CPPFLAGS =  -I$(top_srcdir)/src/extra/pcre
TRE_CPPFLAGS = -I$(top_srcdir)/src/extra
XDR_CPPFLAGS = # -I$(top_srcdir)/src/extra/xdr
XZ_CPPFLAGS =   -I$(top_srcdir)/src/extra/xz/api

ALL_CPPFLAGS = $(ZLIB_CPPFLAGS) $(BZLIB_CPPFLAGS) $(PCRE_CPPFLAGS) \
  $(TRE_CPPFLAGS) $(XDR_CPPFLAGS) $(XZ_CPPFLAGS) $(R_XTRA_CPPFLAGS) $(CPPFLAGS) $(DEFS)
#ALL_CFLAGS = $(ALL_CFLAGS_LO)
#ALL_FFLAGS = $(ALL_FFLAGS_LO)


## use an explicit library: there might be an unsatisfactory -lz around
R_ZLIBS =  ../extra/zlib/libz.a
R_BZLIBS =  ../extra/bzip2/libbz2.a
R_PCRE =  ../extra/pcre/libpcre.a
R_TRE = ../extra/tre/libtre.a
R_XDR = # ../extra/xdr/libxdr.a
R_XZ =   ../extra/xz/liblzma.a
R_LIBINTL = # ../extra/intl/libintl.a

MAIN_LIBS = ../unix/libunix.a ../appl/libappl.a ../nmath/libnmath.a
EXTRA_STATIC_LIBS = \
  $(R_ZLIBS) $(R_BZLIBS) $(R_PCRE) $(R_TRE) $(R_XDR) $(R_XZ) $(R_LIBINTL)
STATIC_LIBS = $(MAIN_LIBS) $(EXTRA_STATIC_LIBS)

EXTRA_LIBS = $(BLAS_LIBS) $(FLIBS) $(R_XTRA_LIBS)  $(READLINE_LIBS) $(LIBS)

R_binary = R.bin
R_bin_OBJECTS = Rmain.o
R_bin_LDADD = libR.a $(EXTRA_LIBS)
## Linked against -lRblas becasue -lR is and otherwise ld complains.
#R_bin_LDADD = -L../../lib -lR -lRblas
R_bin_DEPENDENCIES = libR.a # $(top_builddir)/etc/R.exp

libR_la = libR$(DYLIB_EXT)
## have to force in Rembedded.o
libR_la_OBJECTS = $(OBJECTS) ../unix/Rembedded.o
libR_la_LIBADD = $(STATIC_LIBS) $(EXTRA_LIBS) # # -Wl,-bE:$(top_builddir)/etc/R.exp
libR_la_DEPENDENCIES = $(STATIC_LIBS) # # $(top_builddir)/etc/R.exp

## The next is needed on darwin only at present
LIBR_LDFLAGS = 


all: Makefile Makedeps R

Makefile: $(srcdir)/Makefile.in \
  $(top_builddir)/config.status \
  ../include/config.h \
  ../include/Rversion.h \
  $(SOURCES_C) $(EXTRA_SOURCES_C)
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile $(DEPENDS)
	@cat $(DEPENDS) >> Makefile
	@touch $@

## This target has been changed to ensure that R.bin and libR get
## installed in the build tree if necessary, even if the corresponding 
## objects are not re-made.
## Note that dependencies in the test Makefiles used to depend on these objects,
## so copy-if-change is used.
R: Makefile
	@$(MAKE) Makedeps
#	@$(MAKE) install-lib-local
#	@$(MAKE) install-static-local
	@$(MAKE) install-bin-local


## is this portable?  Documented as GNU extension.
../include/config.h ../include/Rversion.h:
	(cd $(@D); $(MAKE) $(@F))

$(R_binary): $(R_bin_OBJECTS) $(R_bin_DEPENDENCIES)
	$(MAIN_LINK) -o $@ $(R_bin_OBJECTS) $(R_bin_LDADD)

libR.a: $(OBJECTS) $(STATIC_LIBS)
	-@mkdir libs
	@(cd libs; for l in $(STATIC_LIBS); do $(AR) x ../$$l; done)
	@rm -rf $@
	$(AR) cr $@ $(OBJECTS) libs/*o
	$(RANLIB) $@
	@rm -rf libs

install-bin-local: $(R_binary)
	@$(MAKE) rhome="$(abs_top_builddir)" install-bin

install-static-local: libR.a
	@$(MAKE) rhome="$(abs_top_builddir)" install-static

$(top_builddir)/etc/R.exp: $(OBJECTS) $(MAIN_LIBS)
	@$(SHELL) $(top_srcdir)/tools/ldAIX4 -o $@ $(OBJECTS) $(MAIN_LIBS)

## is this portable?  Documented as GNU extension.
$(MAIN_LIBS):
	(cd $(@D); $(MAKE) $(@F))

../extra/bzip2/libbz2.a ../extra/pcre/libpcre.a ../extra/pcre/libtre.a\
  ../extra/xdr/libxdr.a ../extra/zlib/libz.a ../extra/xv/liblzma.a \
  ../extra/intl/libintl.a:
	(cd $(@D); $(MAKE))


libR: $(libR_la)

$(libR_la): $(libR_la_OBJECTS) $(libR_la_DEPENDENCIES)
	$(DYLIB_LINK) $(LIBR_LDFLAGS) -o $@ $(libR_la_OBJECTS) $(libR_la_LIBADD)

install-lib-local: $(libR_la)
	@$(MAKE) rhome="$(abs_top_builddir)" install-lib


# suppress #line directives
YFLAGS=-l

$(srcdir)/gram.c: # $(srcdir)/gram.y
	@$(ECHO) "re-making gram.c"
	$(YACC) $(YFLAGS) $(srcdir)/gram.y
	$(SHELL) $(top_srcdir)/tools/move-if-change y.tab.c $(srcdir)/gram.c
$(srcdir)/gramRd.c: # $(srcdir)/gramRd.y
	@$(ECHO) "re-making gramRd.c"
	$(YACC) $(YFLAGS) $(srcdir)/gramRd.y
	$(SHELL) $(top_srcdir)/tools/move-if-change y.tab.c $(srcdir)/gramRd.c	

install: installdirs
	@$(MAKE) install-bin
	@if test -f $(libR_la); then $(MAKE) install-lib; fi
#	@$(MAKE) install-static
installdirs:
	@$(MKINSTALLDIRS) "$(DESTDIR)$(Rexecbindir2)"
	@$(MKINSTALLDIRS) "$(DESTDIR)$(Rexeclibdir)"
install-bin: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_binary) "$(DESTDIR)$(Rexecbindir2)/R"
install-lib: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(libR_la) "$(DESTDIR)$(Rexeclibdir)/$(libR_la)"
install-static: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change libR.a "$(DESTDIR)$(Rexeclibdir)/libR.a"
install-strip: installdirs
	@${INSTALL_PROGRAM} -s $(R_binary) "$(DESTDIR)$(Rexecbindir2)/R"
	@if test -f $(libR_la); then $(MAKE) install-lib; fi
#	@$(MAKE) install-static
	@if test -n "$(STRIP_LIBS)"; then \
	  if test -f $(libR_la); then $(STRIP_LIBS) "$(DESTDIR)$(Rexeclibdir)/$(libR_la)"; fi; \
	fi
	@if test -n "$(STRIP_STATIC_LIBS)"; then \
	  if test -f libR.a; then $(STRIP_STATIC_LIBS) "$(DESTDIR)$(Rexeclibdir)/libR.a"; fi; \
	fi
uninstall:
	@rm -f "$(DESTDIR)$(Rexecbindir)/exec/$(R_ARCH)/R" "$(DESTDIR)$(Rexecbindir)/R.bin"
	@rm -f "$(DESTDIR)$(Rexeclibdir)/libR$(DYLIB_EXT)"
	@rm -f "$(DESTDIR)$(Rexeclibdir)/libR.a"

mostlyclean: clean
clean:
	@-rm -f $(top_builddir)/etc/R.exp
	@-rm -rf .libs _libs
	@-rm -f *core Makedeps *.d *.o *.lo *.la *$(DYLIB_EXT) \
	  $(R_binary) libR.a
distclean: clean
	@-rm -f Makefile
maintainer-clean: distclean
	@$(ECHO) "This command is intended for maintainers to use; it"
	@$(ECHO) "deletes files that may need special rules to rebuild"
	@-rm -f $(srcdir)/gram.c $(srcdir)/gramRd.c

tags: TAGS
TAGS: $(SOURCES) $(EXTRA_SOURCES_C) $(HEADERS)
	etags $(SOURCES) $(EXTRA_SOURCES_C) $(HEADERS)

## Unused targets
info dvi check:

distdir: $(DISTFILES)
	@for f in $(DISTFILES); do \
	  test -f $(distdir)/$${f} \
	    || ln $(srcdir)/$${f} $(distdir)/$${f} 2>/dev/null \
	    || cp -p $(srcdir)/$${f} $(distdir)/$${f}; \
	done
	@if test -f $(srcdir)/TAOCP1997.c ; then \
	  cp -p $(srcdir)/TAOCP1997.c $(distdir)/TAOCP1997.c; \
	fi

## Automagically generated dependencies:

