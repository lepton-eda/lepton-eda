SUBDIRS = m4 intl po include src pixmaps graphman

# The installation directory for these files needs to be decided on.  
# For now, don't install them. AVH 05/25/03
#gedadocdir = ${prefix}/doc/geda
#gedadoc_DATA = \
#	README\
#	COPYING\
#	AUTHORS\
#	ChangeLog\
#	INSTALL\
#	NEWS\
#	TODO\
#	ABOUT-NLS
#EXTRA_DIST = $(gedadoc_DATA)

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

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
                       config.status aclocal.m4 config.guess config.rpath \
	   	       config.sub depcomp mkinstalldirs missing install-sh \
		       stamp-h.in


ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = config.rpath mkinstalldirs autogen.sh 
