#
# ${R_HOME}/src/extra/xdr/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/extra/xdr
R_HOME = $(top_builddir)

include $(top_builddir)/Makeconf

XDR_CPPFLAGS = -I$(srcdir)
ALL_CPPFLAGS = $(XDR_CPPFLAGS) $(R_XTRA_CPPFLAGS) $(CPPFLAGS) $(DEFS)

SOURCES = xdr.c xdr_float.c xdr_mem.c xdr_stdio.c
HEADERS = rpc/auth.h rpc/auth_uni.h rpc/bcopy.h rpc/clnt.h rpc/netdb.h \
  rpc/rpc.h rpc/rpc_msg.h rpc/svc.h rpc/svc_auth.h rpc/types.h rpc/xdr.h
DEPENDS = $(SOURCES:.c=.d)
OBJECTS = $(SOURCES:.c=.o)
#ALL_CFLAGS = $(ALL_CFLAGS_LO) -fvisibility=hidden


distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
## <FIXME>
## HEADERS currently has files in subdir 'rpc' and hence cannot be used
## in DISTFILES.  Need to get this right eventually and adjust distdir
## accordingly.
DISTFILES = \
  README.txt copyrght.txt Makefile.in Makefile.win $(SOURCES)
SUBDIRS_WITH_NO_BUILD = rpc
## </FIXME>

noinst_LIBRARIES = # libxdr.a
libxdr_a_SOURCES = $(SOURCES)
libxdr_a_OBJECTS = $(OBJECTS)


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
	@$(MAKE) $(noinst_LIBRARIES)

libxdr.a: $(libxdr_a_OBJECTS)
	rm -f $@
	$(AR) cr $@ $(libxdr_a_OBJECTS)
	$(RANLIB) $@


mostlyclean: clean
clean:
	@-rm -f Makedeps *.d *.o *.lo *.a
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
	@for d in $(SUBDIRS_WITH_NO_BUILD); do \
	  ((cd $(srcdir); $(TAR) -c -f - $(DISTDIR_TAR_EXCLUDE) $${d}) \
	      | (cd $(distdir); $(TAR) -x -f -)) \
	    || exit 1; \
	done

## Automagically generated dependencies:

