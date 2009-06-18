SUBDIRS = include lib intl src po scheme bitmap scripts examples \
          tests docs data

EXTRA_DIST = BUGS ChangeLog-1.0 autogen.sh config.rpath configure.ac.in \
             icon-theme-installer desktop-i18n

#proto:
#	(cd src ; ./create_proto)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache \
          intl/*.h intl/*.c intl/*.alias intl/*.y intl/*.sin intl/VERSION \
          intl/config.charset intl/Makefile.in intl/ChangeLog \
          po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot


# Special rule to make sure that configure.ac is recreated when configure.ac.in
# changes.
configure.ac: configure.ac.in
	./autogen.sh recreate_configure_only

ACLOCAL_AMFLAGS = -I m4

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
	               config.status aclocal.m4 config.guess config.rpath \
		       config.sub depcomp install-sh missing mkinstalldirs \
		       configure.ac
DISTCHECK_CONFIGURE_FLAGS = --disable-update-desktop-database
