#
# ${R_HOME}/src/library/tools/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/library/tools

include $(top_builddir)/Makeconf

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = DESCRIPTION.in Makefile.in Makefile.win NAMESPACE

pkg = tools

RSRC = `LC_COLLATE=C ls $(srcdir)/R/*.R`

R_EXE = $(top_builddir)/bin/R --vanilla --slave

all: Makefile DESCRIPTION
	@$(ECHO) "building package '$(pkg)'"
	@$(MKINSTALLDIRS) $(top_builddir)/library/$(pkg)
	@$(MAKE) mkR mkdesc2 mkpo
	@$(MAKE) mksrc
	@$(INSTALL_DATA) all.R $(top_builddir)/library/$(pkg)/R/$(pkg)
	@(cat $(srcdir)/R/makeLazyLoad.R; \
	  $(ECHO) "makeLazyLoading(\"$(pkg)\")") | \
	  R_DEFAULT_PACKAGES=NULL LC_ALL=C $(R_EXE) > /dev/null
	@$(ECHO) "tools:::.install_package_description('.', '"$(top_builddir)/library/${pkg}"')" | \
	  R_DEFAULT_PACKAGES=NULL $(R_EXE) > /dev/null


include $(top_srcdir)/share/make/basepkg.mk
