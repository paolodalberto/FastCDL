#
# ${R_HOME}/src/library/grDevices/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/library/grDevices

include $(top_builddir)/Makeconf

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = DESCRIPTION.in Makefile.in Makefile.win NAMESPACE

pkg = grDevices

R_EXE = $(top_builddir)/bin/R --vanilla --slave

RSRC = `LC_COLLATE=C ls $(srcdir)/R/*.R $(srcdir)/R/$(R_OSTYPE)/*.R`
INSTDIRS = afm enc

all: Makefile DESCRIPTION
	@$(ECHO) "building package '$(pkg)'"
	@$(MKINSTALLDIRS) $(top_builddir)/library/$(pkg)
	@$(MAKE) mkR mkdesc mkpo instdirs
	@$(MAKE) mksrc
	@$(MAKE) mklazy


include $(top_srcdir)/share/make/basepkg.mk
