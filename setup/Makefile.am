## Process this file with automake to produce Makefile.in
## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

SUBDIRS = m4 intl po src

setupdocdir = ${prefix}/doc/setup
setupdoc_DATA = \
	README\
	COPYING\
	AUTHORS\
	ChangeLog\
	INSTALL\
	NEWS\
	TODO\
	ABOUT-NLS

EXTRA_DIST = setup.prj $(setupdoc_DATA) autogen.sh

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache \
          intl/*.h intl/*.c intl/*.alias intl/*.y intl/*.sin intl/VERSION \
          intl/config.charset intl/Makefile.in intl/ChangeLog \
          po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot \
          po/Makefile.in.in po/ChangeLog \
          m4/*.m4 

# Copy all the spec files. Of cource, only one is actually used.
dist-hook:
	for specfile in *.spec; do \
		if test -f $$specfile; then \
			cp -p $$specfile $(distdir); \
		fi \
	done

ACLOCAL_AMFLAGS = -I m4

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
                       config.status aclocal.m4 config.guess config.rpath \
                       config.sub depcomp mkinstalldirs missing install-sh \
                       stamp-h.in

