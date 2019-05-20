#
# ${R_HOME}/src/library/tcltk/src/Makefile

pkg = tcltk


srcdir = .
top_srcdir = ../../../..

top_builddir = ../../../..
subdir = src/library/$(pkg)/src
R_HOME = $(top_builddir)
R_SHARE_DIR = $(R_HOME)/share
R_INCLUDE_DIR = $(R_HOME)/include

SOURCES_C = init.c tcltk.c tcltk_unix.c
SOURCES_F =
DEPENDS = $(SOURCES_C:.c=.d)
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_F:.f=.o)

SHLIB = $(pkg).so

PKG_CPPFLAGS = -I/usr/include  -I/usr/X11R6/include
PKG_LIBS = -L/usr/lib64 -ltcl8.4 -L/usr/lib64 -ltk8.4 -L/usr/X11R6/lib64 -lX11 -lm

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
  ../../../../include/R_ext/PrtUtil.h tcltk.h \
  ../../../../include/R_ext/Parse.h ../../../../include/R_ext/Rdynload.h
tcltk.o: tcltk.c tcltk.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  ../../../../include/R_ext/PrtUtil.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h ../../../../include/R_ext/Parse.h \
  ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Constants.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/RS.h
tcltk_unix.o: tcltk_unix.c tcltk.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  ../../../../include/R_ext/PrtUtil.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h ../../../../include/R_ext/Parse.h \
  ../../../../include/R_ext/eventloop.h ../../../../include/R.h \
  ../../../../include/Rconfig.h ../../../../include/R_ext/Constants.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/RS.h \
  ../../../../include/Rinterface.h ../../../../include/R_ext/RStartup.h
