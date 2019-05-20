#
# ${R_HOME}/src/library/grDevices/src/Makefile

pkg = grDevices


srcdir = .
top_srcdir = ../../../..

top_builddir = ../../../..
subdir = src/library/$(pkg)/src
R_HOME = $(top_builddir)
R_SHARE_DIR = $(R_HOME)/share
R_INCLUDE_DIR = $(R_HOME)/include

SOURCES_C = chull.c devNull.c devPicTeX.c devPS.c devQuartz.c init.c # qdBitmap.c qdPDF.c
SOURCES_M = # qdCocoa.m

DEPENDS = $(SOURCES_C:.c=.d) $(SOURCES_M:.m=.d)
OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_M:.m=.o)

SHLIB = $(pkg).so

# need Defn.h etc, and config.h
PKG_CPPFLAGS =-I../../../include -I$(top_srcdir)/src/include -DHAVE_CONFIG_H
#PKG_LIBS=-framework AppKit

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
chull.o: chull.c ../../../include/config.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Applic.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/BLAS.h \
  ../../../../include/R_ext/Complex.h
devNull.o: devNull.c ../../../include/config.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  ../../../../include/R_ext/PrtUtil.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/GraphicsEngine.h \
  ../../../../include/R_ext/GraphicsDevice.h grDevices.h
devPicTeX.o: devPicTeX.c ../../../include/config.h \
  ../../../include/Defn.h ../../../../include/R_ext/Complex.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/PrtUtil.h \
  ../../../../include/R_ext/Print.h ../../../../include/R_ext/Utils.h \
  ../../../include/Rinlinedfuns.h ../../../include/Internal.h \
  ../../../include/Errormsg.h ../../../../include/R_ext/rlocale.h \
  ../../../../include/R_ext/GraphicsEngine.h \
  ../../../../include/R_ext/GraphicsDevice.h ../../../include/Fileio.h \
  grDevices.h
devPS.o: devPS.c ../../../include/config.h ../../../include/Defn.h \
  ../../../../include/R_ext/Complex.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/PrtUtil.h \
  ../../../../include/R_ext/Print.h ../../../../include/R_ext/Utils.h \
  ../../../include/Rinlinedfuns.h ../../../include/Internal.h \
  ../../../include/Errormsg.h ../../../../include/R_ext/Riconv.h \
  ../../../../include/Rmath.h ../../../../include/R_ext/GraphicsEngine.h \
  ../../../../include/R_ext/GraphicsDevice.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rconfig.h \
  ../../../include/Fileio.h grDevices.h ../../../../include/Rversion.h
devQuartz.o: devQuartz.c ../../../include/config.h grDevices.h \
  ../../../../include/Rinternals.h ../../../../include/R_ext/Arith.h \
  ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Error.h ../../../../include/R_ext/Memory.h \
  ../../../../include/R_ext/PrtUtil.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/GraphicsEngine.h \
  ../../../../include/R_ext/GraphicsDevice.h \
  ../../../../include/R_ext/QuartzDevice.h
init.o: init.c ../../../../include/R.h ../../../../include/Rconfig.h \
  ../../../../include/R_ext/Arith.h ../../../../include/R_ext/libextern.h \
  ../../../../include/R_ext/Boolean.h ../../../../include/R_ext/Complex.h \
  ../../../../include/R_ext/Constants.h ../../../../include/R_ext/Error.h \
  ../../../../include/R_ext/Memory.h ../../../../include/R_ext/Print.h \
  ../../../../include/R_ext/Random.h ../../../../include/R_ext/Utils.h \
  ../../../../include/R_ext/RS.h ../../../../include/Rinternals.h \
  ../../../../include/R_ext/PrtUtil.h \
  ../../../../include/R_ext/GraphicsEngine.h \
  ../../../../include/R_ext/GraphicsDevice.h grDevices.h \
  ../../../../include/R_ext/Rdynload.h
