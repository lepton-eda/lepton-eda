
SUBDIRS = src include utils lib scheme examples scripts tests docs 

EXTRA_DIST = BUGS 

proto:
	(cd src ; ./create_proto)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~ 
CLEANFILES = *.log core FILE *~ 
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
                       config.status aclocal.m4
