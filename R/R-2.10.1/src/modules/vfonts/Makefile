#
# ${R_HOME}/src/modules/vfonts/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/modules/vfonts
R_HOME = $(top_builddir)

include $(top_builddir)/Makeconf

SOURCES = g_alab_her.c g_cntrlify.c g_fontdb.c g_her_glyph.c
DEPENDS = $(SOURCES:.c=.d)
OBJECTS = $(SOURCES:.c=.o)
HEADERS = g_cntrlify.h g_control.h g_extern.h g_her_metr.h g_jis.h

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = \
	Makefile.in Makefile.win $(SOURCES) $(HEADERS) \
	dllversion.rc

vfonts_la = vfonts$(SHLIB_EXT)
## AIX needs LIBM
vfonts_la_LIBADD = $(LIBR) $(LIBM) # $(LIBINTL)

ALL_CFLAGS = $(ALL_CFLAGS_LO)

all: Makefile Makedeps R

Makefile: $(srcdir)/Makefile.in \
  $(top_builddir)/config.status \
  $(SOURCES)
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile $(DEPENDS)
	@cat $(DEPENDS) >> Makefile
	@touch $@

R: Makefile
	@$(MAKE) Makedeps
	@$(MAKE) $(vfonts_la)

$(vfonts_la): $(OBJECTS)
	$(SHLIB_LINK) -o $@ $(OBJECTS) $(vfonts_la_LIBADD)
## <FIXME>
## Not sure if we want to do this ...
	@$(MAKE) rhome="$(abs_top_builddir)" install
## </FIXME>

install: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(vfonts_la) "$(DESTDIR)$(Rexecmodulesdir)/$(vfonts_la)"
installdirs:
	@$(MKINSTALLDIRS) "$(DESTDIR)$(Rexecmodulesdir)"
install-strip: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(vfonts_la) "$(DESTDIR)$(Rexecmodulesdir)/$(vfonts_la)"
	@if test -n "$(STRIP_LIBS)"; then \
	  $(STRIP_LIBS) "$(DESTDIR)$(Rexecmodulesdir)/$(vfonts_la)" ;\
	fi
uninstall:
	@rm -f "$(Rexecmodulesdir)/vfonts$(SHLIB_EXT)"

mostlyclean: clean
clean:
	@-rm -rf .libs _libs
	@-rm -f Makedeps *.d *.o *.a *.lo *.la *$(SHLIB_EXT)
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

g_alab_her.o: g_alab_her.c g_extern.h ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  ../../../src/include/R_ext/GraphicsEngine.h \
  ../../../src/include/R_ext/GraphicsDevice.h g_control.h g_her_metr.h \
  ../../../src/include/Rmodules/Rvfonts.h \
  ../../../src/include/R_ext/Rdynload.h
g_cntrlify.o: g_cntrlify.c g_extern.h ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  ../../../src/include/R_ext/GraphicsEngine.h \
  ../../../src/include/R_ext/GraphicsDevice.h g_control.h g_cntrlify.h \
  g_jis.h
g_fontdb.o: g_fontdb.c g_extern.h ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  ../../../src/include/R_ext/GraphicsEngine.h \
  ../../../src/include/R_ext/GraphicsDevice.h
g_her_glyph.o: g_her_glyph.c g_extern.h ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  ../../../src/include/R_ext/GraphicsEngine.h \
  ../../../src/include/R_ext/GraphicsDevice.h
