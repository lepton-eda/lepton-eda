
SUBDIRS = src include utils lib scheme examples scripts docs tests

EXTRA_DIST = BUGS 

proto:
	(cd src ; ./create_proto)

MOSTLYCLEANFILES = *.log core FILE *~ 
CLEANFILES = *.log core FILE *~ 
DISTCLEANFILES = *.log core FILE *~ Makefile.in configure
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure
