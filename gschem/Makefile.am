
SUBDIRS = include lib intl src m4 po scheme bitmap scripts examples noweb \
          tests docs 

EXTRA_DIST = config.rpath VOCABULARY BUGS guile.m4

#proto:
#	(cd src ; ./create_proto)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

ACLOCAL_AMFLAGS = -I m4

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
	               config.status aclocal.m4
