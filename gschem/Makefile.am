
SUBDIRS = src include lib scheme scripts examples docs noweb

EXTRA_DIST = VOCABULARY BUGS guile.m4

proto:
	(cd src ; ./create_proto)

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ Makefile.in configure
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure
