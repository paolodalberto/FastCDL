#
# ${R_HOME}/src/library/methods/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/library/methods

include $(top_builddir)/Makeconf
distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = DESCRIPTION.in Makefile.in Makefile.win NAMESPACE

pkg = methods

RSRC = `LC_COLLATE=C ls $(srcdir)/R/*.R`

R_EXE = $(top_builddir)/bin/R --vanilla --slave

all: Makefile DESCRIPTION
	@$(ECHO) "building package '$(pkg)'"
	@$(MKINSTALLDIRS) $(top_builddir)/library/$(pkg)
	@$(MAKE) mkR2 mkdesc mkpo
	@$(MAKE) mksrc
	@$(MAKE) RfilesLazy

RfilesLazy: $(top_builddir)/library/$(pkg)/R/$(pkg).rdb

## loadNamespace() depends on the NAMESPACE file.  We remove the
## dumped copy (it is rebuilt later), and we have already installed
## NAMESPACE in the library tree.
$(top_builddir)/library/$(pkg)/R/$(pkg).rdb: all.R  $(srcdir)/NAMESPACE
	@rm -f $(top_builddir)/library/$(pkg)/Meta/nsInfo.rds $@
	@$(ECHO) "dumping R code in package '$(pkg)'"
	@$(INSTALL_DATA) all.R $(top_builddir)/library/$(pkg)/R/$(pkg)
	@$(ECHO) "invisible(loadNamespace(\"$(pkg)\"))" | \
	  R_DEFAULT_PACKAGES=NULL LC_ALL=C $(R_EXE)
	@$(INSTALL_DATA) $(top_srcdir)/share/R/nspackloader.R \
	  $(top_builddir)/library/$(pkg)/R/$(pkg)

include $(top_srcdir)/share/make/basepkg.mk
