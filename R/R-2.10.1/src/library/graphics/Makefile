#
# ${R_HOME}/src/library/graphics/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/library/graphics

include $(top_builddir)/Makeconf

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = DESCRIPTION.in Makefile.in Makefile.win NAMESPACE

pkg = graphics

R_EXE = $(top_builddir)/bin/R --vanilla --slave

RSRC = `LC_COLLATE=C ls $(srcdir)/R/*.R`

all: Makefile DESCRIPTION
	@$(ECHO) "building package '$(pkg)'"
	@$(MKINSTALLDIRS) $(top_builddir)/library/$(pkg)
	@$(MAKE) mkR mkdesc mkdemos mkpo
	@$(MAKE) mklazy


include $(top_srcdir)/share/make/basepkg.mk

