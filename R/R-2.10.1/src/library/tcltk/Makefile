#
# ${R_HOME}/src/library/tcltk/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/library/tcltk

include $(top_builddir)/Makeconf

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = DESCRIPTION.in Makefile.in Makefile.win NAMESPACE

pkg = tcltk

R_EXE = $(top_builddir)/bin/R --vanilla --slave

RSRC = `LC_COLLATE=C ls $(srcdir)/R/*.R $(srcdir)/R/$(R_OSTYPE)/zzz.R`

all: Makefile DESCRIPTION
	@$(ECHO) "building package '$(pkg)'"
	@$(MKINSTALLDIRS) $(top_builddir)/library/$(pkg)/R
	@(f=$${TMPDIR:-/tmp}/R$$$$; \
	  if test yes = yes; then \
	    cat $(RSRC) > $${f}; \
	    $(SHELL) $(top_srcdir)/tools/move-if-change $${f} all.R ; \
	  else \
	    $(SHELL) $(top_srcdir)/tools/copy-if-change \
	      $(srcdir)/R/unix/zzzstub.R \
	      $(top_builddir)/library/$(pkg)/R/$(pkg) ; \
	  fi)
	@if test yes = yes; then \
	  $(INSTALL_DATA) $(srcdir)/NAMESPACE \
	    $(top_builddir)/library/$(pkg); \
	  rm -f $(top_builddir)/library/$(pkg)/Meta/nsInfo.rds; \
	fi;
	@$(MAKE) mkdesc mkexec mkdemos mkpo
	@if test -d src -a yes = yes; then \
	  (cd src && $(MAKE)) || exit 1; \
	fi
	@if test yes = yes; then \
	  $(INSTALL_DATA) all.R $(top_builddir)/library/$(pkg)/R/$(pkg) ; \
	  $(ECHO) "tools:::makeLazyLoading(\"$(pkg)\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_ALL=C $(R_EXE) > /dev/null ; \
	fi


include $(top_srcdir)/share/make/basepkg.mk
