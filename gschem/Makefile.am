SUBDIRS = include lib intl src m4 po scheme bitmap scripts examples \
          tests docs data

INTLTOOL_FILES = intltool-extract.in \
                 intltool-merge.in \
                 intltool-update.in

EXTRA_DIST = BUGS ChangeLog-1.0 autogen.sh config.rpath configure.ac.in \
             icon-theme-installer $(INTLTOOL_FILES)

#proto:
#	(cd src ; ./create_proto)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache \
          intl/*.h intl/*.c intl/*.alias intl/*.y intl/*.sin intl/VERSION \
          intl/config.charset intl/Makefile.in intl/ChangeLog \
          po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot \
          po/Makefile.in.in po/ChangeLog \
          m4/*.m4 

ACLOCAL_AMFLAGS = -I m4

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak \
                 intltool-extract \
                 intltool-merge \
                 intltool-update \
                 po/.intltool-merge-cache
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
	               config.status aclocal.m4 config.guess config.rpath \
		       config.sub depcomp install-sh missing mkinstalldirs \
		       configure.ac $(INTLTOOL_FILES)
DISTCHECK_CONFIGURE_FLAGS = --disable-update-desktop-database
