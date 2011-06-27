
SUBDIRS = po data docs include lib src shell scheme share

EXTRA_DIST = HACKING BUGS ChangeLog ChangeLog-1.0 po/domain.mak.in

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libgeda.pc

libgeda-pc-install: libgeda.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libgeda.pc $(DESTDIR)$(pkgconfigdir)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ libgeda.pc po/domain.mak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in libgeda.pc ChangeLog
