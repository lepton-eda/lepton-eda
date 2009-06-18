
SUBDIRS = po data docs include intl lib scheme share src

EXTRA_DIST = HACKING BUGS ChangeLog-1.0 autogen.sh configure.ac.in \
             icon-theme-installer desktop-i18n po/domain.mak.in

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libgeda.pc

libgeda-pc-install: libgeda.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libgeda.pc $(DESTDIR)$(pkgconfigdir)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

# Special rule to make sure that configure.ac is recreated when configure.ac.in
# changes.
configure.ac: configure.ac.in
	./autogen.sh recreate_configure_only

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak libgeda.pc po/domain.mak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
	               config.status aclocal.m4 libgeda.pc config.guess \
		       config.sub depcomp install-sh ltmain.sh missing \
		       mkinstalldirs configure.ac

DISTCHECK_CONFIGURE_FLAGS = --disable-update-mime-database

ACLOCAL_AMFLAGS = -I m4
