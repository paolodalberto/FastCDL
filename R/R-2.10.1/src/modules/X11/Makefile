#
# ${R_HOME}/src/modules/X11/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/modules/X11
R_HOME = $(top_builddir)

include $(top_builddir)/Makeconf

ALL_CPPFLAGS = $(R_XTRA_CPPFLAGS) $(X_CFLAGS)  $(CPPFLAGS) $(DEFS)
ALL_CFLAGS = $(ALL_CFLAGS_LO)
ALL_X_LIBS = $(X_PRE_LIBS)  $(X_LIBS) $(X_EXTRA_LIBS)

SOURCES = dataentry.c devX11.c rotated.c rbitmap.c 
DEPENDS = $(SOURCES:.c=.d)
OBJECTS = $(SOURCES:.c=.o)
HEADERS = cairoX11.c devX11.h rotated.h

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = \
	Makefile.in $(SOURCES) $(HEADERS)

R_X11_la = R_X11$(SHLIB_EXT)
## AIX needs LIBM
R_X11_la_LIBADD = $(ALL_X_LIBS) $(BITMAP_LIBS) $(LIBR) $(LIBM) # $(LIBINTL)

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
	@$(MAKE) $(R_X11_la)

$(R_X11_la): $(OBJECTS)
	$(SHLIB_LINK) -o $@ $(OBJECTS) $(R_X11_la_LIBADD)
## <FIXME>
## Not sure of we want to do this ...
	@$(MAKE) rhome="$(abs_top_builddir)" install
## </FIXME>

install: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_X11_la) "$(DESTDIR)$(Rexecmodulesdir)/$(R_X11_la)"
installdirs:
	@$(MKINSTALLDIRS) "$(DESTDIR)$(Rexecmodulesdir)"
install-strip: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_X11_la) "$(DESTDIR)$(Rexecmodulesdir)/$(R_X11_la)"
	@if test -n "$(STRIP_LIBS)"; then \
	  $(STRIP_LIBS) "$(DESTDIR)$(Rexecmodulesdir)/$(R_X11_la)" ;\
	fi
uninstall:
	@rm -f "$(Rexecmodulesdir)/R_X11$(SHLIB_EXT)"

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

dataentry.o: dataentry.c ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  ../../../src/include/R_ext/Parse.h /usr/X11R6/include/X11/X.h \
  /usr/X11R6/include/X11/Xlib.h /usr/X11R6/include/X11/Xfuncproto.h \
  /usr/X11R6/include/X11/Xosdefs.h /usr/X11R6/include/X11/Xutil.h \
  /usr/X11R6/include/X11/keysym.h /usr/X11R6/include/X11/keysymdef.h \
  /usr/X11R6/include/X11/cursorfont.h /usr/X11R6/include/X11/Intrinsic.h \
  /usr/X11R6/include/X11/Xresource.h /usr/X11R6/include/X11/Core.h \
  /usr/X11R6/include/X11/Composite.h /usr/X11R6/include/X11/Constraint.h \
  /usr/X11R6/include/X11/Object.h /usr/X11R6/include/X11/RectObj.h \
  ../../../src/include/Print.h ../../../src/include/Defn.h \
  ../../../src/include/R_ext/eventloop.h ../../../src/include/R_ext/RS.h \
  ../../../src/include/Rconfig.h ../../../src/include/R_ext/rlocale.h \
  /usr/X11R6/include/X11/StringDefs.h /usr/X11R6/include/X11/Shell.h \
  /usr/X11R6/include/X11/SM/SMlib.h /usr/X11R6/include/X11/SM/SM.h \
  /usr/X11R6/include/X11/ICE/ICElib.h /usr/X11R6/include/X11/ICE/ICE.h
devX11.o: devX11.c ../../../src/include/config.h \
  ../../../src/include/Defn.h ../../../src/include/R_ext/Complex.h \
  ../../../src/include/Rinternals.h ../../../src/include/R_ext/Arith.h \
  ../../../src/include/R_ext/libextern.h \
  ../../../src/include/R_ext/Boolean.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Memory.h \
  ../../../src/include/R_ext/PrtUtil.h ../../../src/include/R_ext/Print.h \
  ../../../src/include/R_ext/Utils.h ../../../src/include/Rinlinedfuns.h \
  ../../../src/include/Internal.h ../../../src/include/Errormsg.h \
  /usr/X11R6/include/X11/X.h /usr/X11R6/include/X11/Xlib.h \
  /usr/X11R6/include/X11/Xfuncproto.h /usr/X11R6/include/X11/Xosdefs.h \
  /usr/X11R6/include/X11/Xatom.h /usr/X11R6/include/X11/Xutil.h \
  /usr/X11R6/include/X11/cursorfont.h /usr/X11R6/include/X11/Intrinsic.h \
  /usr/X11R6/include/X11/Xresource.h /usr/X11R6/include/X11/Core.h \
  /usr/X11R6/include/X11/Composite.h /usr/X11R6/include/X11/Constraint.h \
  /usr/X11R6/include/X11/Object.h /usr/X11R6/include/X11/RectObj.h \
  /usr/X11R6/include/X11/Xmu/Atoms.h \
  ../../../src/include/R_ext/GraphicsEngine.h \
  ../../../src/include/R_ext/GraphicsDevice.h \
  ../../../src/include/Fileio.h rotated.h \
  ../../../src/include/R_ext/eventloop.h devX11.h \
  ../../../src/include/Rmodules/RX11.h \
  ../../../src/include/Rconnections.h /usr/X11R6/include/X11/StringDefs.h \
  /usr/X11R6/include/X11/Shell.h /usr/X11R6/include/X11/SM/SMlib.h \
  /usr/X11R6/include/X11/SM/SM.h /usr/X11R6/include/X11/ICE/ICElib.h \
  /usr/X11R6/include/X11/ICE/ICE.h ../../../src/include/R_ext/Rdynload.h
rotated.o: rotated.c ../../../src/include/config.h \
  /usr/X11R6/include/X11/Xlib.h /usr/X11R6/include/X11/X.h \
  /usr/X11R6/include/X11/Xfuncproto.h /usr/X11R6/include/X11/Xosdefs.h \
  /usr/X11R6/include/X11/Xutil.h /usr/X11R6/include/X11/Xatom.h rotated.h
rbitmap.o: rbitmap.c ../../../src/include/config.h \
  ../../../src/include/R_ext/Error.h
