
SUBDIRS = po data docs include lib scheme share src

EXTRA_DIST = HACKING BUGS ChangeLog-1.0 po/domain.mak.in

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libgeda.pc

libgeda-pc-install: libgeda.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libgeda.pc $(DESTDIR)$(pkgconfigdir)

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ libgeda.pc po/domain.mak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in libgeda.pc
