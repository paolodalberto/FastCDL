#
# ${R_HOME}/src/library/base/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/library/base

include $(top_builddir)/Makeconf

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = DESCRIPTION.in Makefile.in Makefile.win baseloader.R makebasedb.R

pkg = base

RSRC = `LC_COLLATE=C ls $(srcdir)/R/*.R $(srcdir)/R/$(R_OSTYPE)/*.R`

R_EXE = $(top_builddir)/bin/R --vanilla --slave

all: Makefile DESCRIPTION
	@$(ECHO) "building package '$(pkg)'"
	@$(MKINSTALLDIRS) $(top_builddir)/library/$(pkg)
	@$(MAKE) mkR mkdesc2 mkdemos2 mkpo
	@$(INSTALL_DATA) $(srcdir)/inst/CITATION $(top_builddir)/library/$(pkg)
	@cat $(srcdir)/makebasedb.R | \
	  R_DEFAULT_PACKAGES=NULL LC_ALL=C $(R_EXE) > /dev/null
	@$(INSTALL_DATA) $(srcdir)/baseloader.R \
	  $(top_builddir)/library/$(pkg)/R/$(pkg)

include $(top_srcdir)/share/make/basepkg.mk
