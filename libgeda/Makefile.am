
SUBDIRS = src include scripts docs share

EXTRA_DIST = HACKING BUGS autogen.sh

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libgeda.pc

libgeda-pc-install: libgeda.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libgeda.pc $(DESTDIR)$(pkgconfigdir)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak libgeda.pc
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
	               config.status aclocal.m4 libgeda.pc config.guess \
		       config.sub depcomp install-sh ltmain.sh missing \
		       mkinstalldirs

