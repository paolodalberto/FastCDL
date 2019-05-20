#
# ${R_HOME}/src/library/tools/src/Makefile

pkg = tools


srcdir = .
top_srcdir = ../../../..

top_builddir = ../../../..
subdir = src/library/$(pkg)/src
R_HOME = $(top_builddir)
R_SHARE_DIR = $(R_HOME)/share
R_INCLUDE_DIR = $(R_HOME)/include

SOURCES_C = text.c init.c Rmd5.c md5.c
SOURCES_F =
DEPENDS = $(SOURCES_C:.c=.d)
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_F:.f=.o)

PKG_CFLAGS=$(C_VISIBILITY)

SHLIB = $(pkg).so

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
text.o: text.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h tools.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h
init.o: init.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h tools.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h \
  ../../../../include/R_ext/Rdynload.h
Rmd5.o: Rmd5.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h tools.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h md5.h
md5.o: md5.c ../../../../include/Rconfig.h md5.h
